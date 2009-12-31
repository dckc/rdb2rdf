package w3c.sw

import scala.util.parsing.combinator._

object SQLParsers extends RegexParsers {

  val int = """[0-9]+""".r
  val chars = "\"[^\"]*\"".r
}

import SQLParsers._

import scala.util.parsing.combinator._
import java.net.URI

case class Union(disjoints:Set[Select]) {
  override def toString = (disjoints mkString ("\nUNION\n"))
}
case class Select(attributelist:AttributeList, tablelist:TableList, expression:Expression) {
  override def toString = attributelist+"\n"+tablelist+"\n"+expression
}
case class AttributeList(attributes:Set[NamedAttribute]) {
  // foo, bar
  override def toString = "SELECT "+(attributes mkString (",\n       "))
}
case class NamedAttribute(value:RelAliasAttributeORConst, attralias:AttrAlias) {
  override def toString = value + " AS " + attralias
}
//case class RelAttribute(relation:Relation, attribute:Attribute) c.f. ForeignKey
sealed abstract class RelAliasAttributeORConst
case class RelAliasAttribute(relalias:RelAlias, attribute:Attribute) extends RelAliasAttributeORConst {
  override def toString = relalias + "." + attribute
}
sealed abstract class Const  extends RelAliasAttributeORConst
case class ConstNULL() extends Const {
  override def toString = "NULL"
}
case class ConstInt(i:String) extends Const {
  override def toString = "" + i
}
case class ConstChars(s:String) extends Const {
  override def toString = "\"" + s + "\""
}

case class Attribute(n:Name) {
  override def toString = n.s /* "'" + n.s + "'" */
}
case class AttrAlias(n:Name) {
  override def toString = n.s /* "'" + n.s + "'" */
}
sealed abstract class RelationORSubselect
case class Relation(n:Name) extends RelationORSubselect {
  override def toString = n.s /* "'" + n.s + "'" */
}
case class Subselect(union:Union) extends RelationORSubselect {
  override def toString = "(\n" + union + "\n)"
}
case class RelAlias(n:Name) {
  override def toString = n.s /* "'" + n.s + "'" */
}
case class TableList(joins:Set[AliasedResource]) {
  override def toString = "  FROM " + (joins mkString ("\n       INNER JOIN "))
}
case class AliasedResource(rel:RelationORSubselect, as:RelAlias) {
  override def toString = rel + " AS " + as
}
case class Expression(conjuncts:Set[PrimaryExpression]) {
  override def toString = " WHERE " + (conjuncts mkString ("\n       AND "))
}
sealed abstract class PrimaryExpression
case class PrimaryExpressionEq(l:RelAliasAttribute, r:RValue) extends PrimaryExpression {
  override def toString = l + "=" + r
}
case class PrimaryExpressionLt(l:RelAliasAttribute, r:RValue) extends PrimaryExpression {
  override def toString = l + "<" + r
}
case class PrimaryExpressionNotNull(l:RelAliasAttribute) extends PrimaryExpression {
  override def toString = l + " IS NOT NULL"
}
sealed abstract class RValue
case class RValueAttr(fqattribute:RelAliasAttribute) extends RValue {
  override def toString = "" + fqattribute
}
case class RValueTyped(datatype:SQLDatatype, i:Name) extends RValue {
  override def toString = i.s /* "'" + i.s + "'" */ /* + datatype */
}
case class Name(s:String)

object Name {
  implicit def fromStringToName(s:String):Name = Name(s)
}

case class SQLDatatype(name:String) {
  override def toString = "/* " + name + " */"
}
object SQLDatatype {
  val STRING = SQLDatatype("String")
  val INTEGER = SQLDatatype("Int")
  val DATE = SQLDatatype("Date")
}

sealed abstract class ValueDescription
case class Value(datatype:SQLDatatype) extends ValueDescription
case class ForeignKey(rel:Relation, attr:Attribute) extends ValueDescription

case class DatabaseDesc(relationdescs:Map[Relation,RelationDesc])
case class RelationDesc(primarykey:Option[Attribute], attributes:Map[Attribute, ValueDescription])

case class Sql() extends JavaTokenParsers {

  def union:Parser[Union] =
    repsep(select, "UNION") ^^ { l => Union(l.toSet) }

  def select:Parser[Select] =
    "SELECT" ~ attributelist ~ "FROM" ~ tablelist ~ opt(where) ^^
    {
      case "SELECT" ~ attributes ~ "FROM" ~ tables ~ whereexpr => {
	val expression:Expression = whereexpr match {
	  case None => Expression(Set())
	  case Some(f) => f
	}
	Select(attributes, tables, expression)
      }
    }

  def where:Parser[Expression] =
    "WHERE" ~ expression ^^ { case "WHERE" ~ expression => expression }

  def attributelist:Parser[AttributeList] =
    repsep(namedattribute, ",") ^^ { l => AttributeList(l.toSet) }

  def namedattribute:Parser[NamedAttribute] =
    fqattributeORconst ~ "AS" ~ attralias ^^
    { case fqattributeORconst ~ "AS" ~ attralias =>
      NamedAttribute(fqattributeORconst, attralias) }

  def fqattributeORconst:Parser[RelAliasAttributeORConst] = (
      fqattribute ^^ { case fqattribute => fqattribute }
    | const ^^ { case const => const }
  )

  def fqattribute:Parser[RelAliasAttribute] =
    relalias ~ "." ~ attribute ^^
    { case relalias ~ "." ~ attribute => RelAliasAttribute(relalias, attribute) }

  def const:Parser[Const] = (
      "NULL" ^^ { case "NULL" => ConstNULL() }
    | int ^^ { case i => ConstInt(i) }
    | chars ^^ { case ch => ConstChars(ch) }
  )

  def attribute:Parser[Attribute] =
    """[a-zA-Z_]\w*""".r ^^ { x => Attribute(Name(x)) }

  def attralias:Parser[AttrAlias] =
    """[a-zA-Z_]\w*""".r ^^ { x => AttrAlias(Name(x)) }

  def relationORsubselect:Parser[RelationORSubselect] = (
      """[a-zA-Z_]\w*""".r ^^ { x => Relation(Name(x)) }
    | "(" ~ union ~ ")" ^^ { case "("~s~")" => Subselect(s) }
  )

  def relalias:Parser[RelAlias] =
    """[a-zA-Z_]\w*""".r ^^ { x => RelAlias(Name(x)) }

  def tablelist:Parser[TableList] =
    repsep(aliasedjoin, "INNER" ~ "JOIN") ^^ { m => TableList(m.toSet) }

  def aliasedjoin:Parser[AliasedResource] =
    relationORsubselect ~ "AS" ~ relalias ^^
    { case rel1 ~ "AS" ~ rel2 => AliasedResource(rel1, rel2) }

  def expression:Parser[Expression] = 
    repsep(primaryexpression, "AND") ^^ 
    { m => Expression(m.toSet) }

  def primaryexpression:Parser[PrimaryExpression] = (
      fqattribute ~ "=" ~ rvalue ^^
      { case fqattribute ~ "=" ~ rvalue => PrimaryExpressionEq(fqattribute, rvalue) }
    | fqattribute ~ "<" ~ rvalue ^^
      { case fqattribute ~ "<" ~ rvalue => PrimaryExpressionLt(fqattribute, rvalue) }
    | fqattribute ~ "IS" ~ "NOT" ~ "NULL" ^^
      { case fqattribute ~ "IS" ~ "NOT" ~ "NULL" => PrimaryExpressionNotNull(fqattribute) }
  )

  def rvalue:Parser[RValue] = (
      fqattribute ^^ { RValueAttr(_) }
    | """[0-9]+""".r ^^ { x => RValueTyped(SQLDatatype.INTEGER, Name(x)) }
    | "\"[^\"]*\"".r  ^^ { x => RValueTyped(SQLDatatype.STRING, Name(x.substring(1, x.size - 1))) }
  )

}

