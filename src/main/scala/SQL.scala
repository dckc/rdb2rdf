package w3c.sw.sql
import  w3c.sw.util._

import scala.util.parsing.combinator._

object SQLParsers extends RegexParsers {

  val int = """[0-9]+""".r
  val chars = "\"([^\"\\\\\n\r]|\\\\[tbnrf\\\"'])*\"".r
}

import SQLParsers._

import scala.util.parsing.combinator._
import java.net.URI

sealed abstract class RelationORSubselect
case class Subselect(sel:SelectORUnion) extends RelationORSubselect {
  override def toString = "(\n" + sel + "\n                       )"
}
sealed abstract class SelectORUnion
case class Select(attributelist:AttributeList, tablelist:TableList, expression:Option[Expression]) extends SelectORUnion {
  override def toString = expression match {
    case Some(expr) => attributelist+"\n"+tablelist+"\n WHERE "+expr
    case None => attributelist+"\n"+tablelist
  }
}
case class Relation(n:Name) extends RelationORSubselect {
  override def toString = n.s /* "'" + n.s + "'" */
}
case class Union(disjoints:Set[Select]) extends SelectORUnion {
  override def toString = "\n" + (disjoints mkString ("\nUNION\n")) + "\n)"
}
case class AttributeList(attributes:Set[NamedAttribute]) {
  // foo, bar
  override def toString = "SELECT "+(attributes mkString (",\n       "))
}
case class NamedAttribute(value:RelAliasAttributeORExpression, attralias:AttrAlias) {
  override def toString = value + " AS " + attralias
}
//case class RelAttribute(relation:Relation, attribute:Attribute) c.f. ForeignKey
sealed abstract class RelAliasAttributeORExpression
case class RelAliasAttribute(relalias:RelAlias, attribute:Attribute) extends RelAliasAttributeORExpression {
  override def toString = relalias + "." + attribute
}

case class Attribute(n:Name) {
  override def toString = n.s /* "'" + n.s + "'" */
}
case class AttrAlias(n:Name) {
  override def toString = n.s /* "'" + n.s + "'" */
}
case class RelAlias(n:Name) {
  override def toString = n.s /* "'" + n.s + "'" */
}
case class TableList(joins:AddOrderedSet[Join]) {
  override def toString = "  FROM " + joins.foldLeft(("", 0))(
    (pair, entry) => (pair._1 + {
      if (pair._2 == 0) entry.toString.substring(19) // !!! shameless!
      else entry
    }, pair._2+1))._1
}

sealed abstract class Join(res:AliasedResource)
case class InnerJoin(res:AliasedResource) extends Join(res) {
  override def toString = "\n       INNER JOIN " + res
}
case class LeftOuterJoin(res:AliasedResource, on:Expression) extends Join(res) {
  override def toString = "\n       LEFT OUTER JOIN " + res + " ON " + on
}

case class AliasedResource(rel:RelationORSubselect, as:RelAlias) {
  override def toString = rel + " AS " + as
}
sealed abstract class Expression extends RelAliasAttributeORExpression
case class ExprConjunction(exprs:Set[Expression]) extends Expression {
  override def toString = "(" + (exprs mkString (")\n       AND (")) + ")"
}
case class ExprDisjunction(exprs:Set[Expression]) extends Expression {
  override def toString = "(" + (exprs mkString (") OR (")) + ")"
}
sealed abstract class RelationalExpression extends Expression
case class RelationalExpressionEq(l:Expression, r:Expression) extends RelationalExpression {
  override def toString = l + "=" + r
}
case class RelationalExpressionNe(l:Expression, r:Expression) extends RelationalExpression {
  override def toString = l + "!=" + r
}
case class RelationalExpressionLt(l:Expression, r:Expression) extends RelationalExpression {
  override def toString = l + "<" + r
}
case class RelationalExpressionNull(l:Expression) extends RelationalExpression { // Expression?
  override def toString = l + " IS NULL"
}
case class RelationalExpressionNotNull(l:Expression) extends RelationalExpression { // Expression?
  override def toString = l + " IS NOT NULL"
}
sealed abstract class PrimaryExpression extends Expression
case class PrimaryExpressionAttr(fqattribute:RelAliasAttribute) extends PrimaryExpression {
  override def toString = "" + fqattribute
}
case class PrimaryExpressionTyped(datatype:Datatype, i:Name) extends PrimaryExpression {
  override def toString = i.s /* "'" + i.s + "'" */ /* + datatype */
}
case class ConstNULL() extends PrimaryExpression {
  override def toString = "NULL"
}
case class Concat(args:List[Expression]) extends PrimaryExpression {
  override def toString = args.mkString("CONCAT(", ", ", ")")
}

case class Name(s:String)

object Name {
  implicit def fromStringToName(s:String):Name = Name(s)
}

case class Datatype(name:String) {
  override def toString = "/* " + name + " */"
}
object Datatype {
  val STRING = Datatype("String")
  val INTEGER = Datatype("Int")
  val DATE = Datatype("Date")
}

sealed abstract class ValueDescription
case class Value(datatype:Datatype) extends ValueDescription
case class ForeignKey(rel:Relation, attr:Attribute) extends ValueDescription

case class DatabaseDesc(relationdescs:Map[Relation,RelationDesc])
case class RelationDesc(primarykey:Option[Attribute], attributes:Map[Attribute, ValueDescription])

case class Sql() extends JavaTokenParsers {

  def selectORunion:Parser[SelectORUnion] =
    rep1sep(select, "UNION") ^^ { l => if (l.size == 1) l(0) else Union(l.toSet) }

  def select:Parser[Select] =
    "SELECT" ~ attributelist ~ "FROM" ~ tablelist ~ opt(where) ^^
    {
      case "SELECT" ~ attributes ~ "FROM" ~ tablesANDons ~ whereexpr => {
	val t:Set[Expression] = tablesANDons._2
	val onConjoints = tablesANDons._2.foldLeft(Set[Expression]())((set, ent) =>
	  ent match {
	    case ExprConjunction(l) => l
	    case _ => Set(ent)
	  })
	val conjoints = whereexpr match {
	  case Some(ExprConjunction(l)) => onConjoints ++ l
	  case Some(x) => onConjoints + x
	  case _ => onConjoints
	}
	val expr:Option[Expression] = conjoints.size match {
	  case 0 => None
	  case 1 => Some(conjoints.toList(0))
	  case _ => Some(ExprConjunction(conjoints))
	}
	Select(attributes, tablesANDons._1, expr)
      }
    }

  def where:Parser[Expression] =
    "WHERE" ~ expression ^^ { case "WHERE" ~ expression => expression }

  def attributelist:Parser[AttributeList] =
    repsep(namedattribute, ",") ^^ { l => AttributeList(l.toSet) }

  def namedattribute:Parser[NamedAttribute] =
    fqattributeORprimaryexpression ~ "AS" ~ attralias ^^
    { case fqattributeORprimaryexpression ~ "AS" ~ attralias =>
      NamedAttribute(fqattributeORprimaryexpression, attralias) }

  def fqattributeORprimaryexpression:Parser[RelAliasAttributeORExpression] = (
      fqattribute ^^ { case fqattribute => fqattribute }
    | primaryexpression ^^ { case const => const }
  )

  def fqattribute:Parser[RelAliasAttribute] =
    relalias ~ "." ~ attribute ^^
    { case relalias ~ "." ~ attribute => RelAliasAttribute(relalias, attribute) }

  def attribute:Parser[Attribute] =
    """[a-zA-Z_]\w*""".r ^^ { x => Attribute(Name(x)) }

  def attralias:Parser[AttrAlias] =
    """[a-zA-Z_]\w*""".r ^^ { x => AttrAlias(Name(x)) }

  def relationORsubselect:Parser[RelationORSubselect] = (
      """[a-zA-Z_]\w*""".r ^^ { x => Relation(Name(x)) }
    | "(" ~ selectORunion ~ ")" ^^ { case "("~s~")" => Subselect(s) }
  )

  def relalias:Parser[RelAlias] =
    """[a-zA-Z_]\w*""".r ^^ { x => RelAlias(Name(x)) }

  def tablelist:Parser[(TableList, Set[Expression])] =
    aliasedjoin ~ rep(innerORouter) ^^ { case aj~l => (TableList(AddOrderedSet(InnerJoin(aj) :: l.map((one) => one._1))), 
						       l.foldLeft(Set[Expression]())((all, one) => all ++ one._2)) }

  def innerORouter:Parser[(Join, Set[Expression])] = (
      "INNER" ~ "JOIN" ~ aliasedjoin ~ opt("ON" ~ expression) ^^
      { case "INNER"~"JOIN"~a~o => (InnerJoin(a), { if (o.isDefined) Set(o.get._2) else Set[Expression]() } ) }
    | "LEFT" ~ "OUTER" ~ "JOIN" ~ aliasedjoin ~ "ON" ~ expression ^^
      { case l~o~j~alijoin~on~expr => (LeftOuterJoin(alijoin, expr), Set[Expression]()) }
  )

  def aliasedjoin:Parser[AliasedResource] =
    relationORsubselect ~ "AS" ~ relalias ^^
    { case rel1 ~ "AS" ~ rel2 => AliasedResource(rel1, rel2) }

  def expression:Parser[Expression] =
    ORexpression ^^ { x => x }

  def ORexpression:Parser[Expression] =
    rep1sep (ANDexpression, "OR") ^^ 
    { xs => if (xs.size > 1) ExprDisjunction(xs.toSet) else xs(0) }

  def ANDexpression:Parser[Expression] =
    rep1sep (relationalexpression, "AND") ^^ 
    { xs => if (xs.size > 1) ExprConjunction(xs.toSet) else xs(0) }

  def relationalexpression:Parser[Expression] = (
      primaryexpression ~ "=" ~ primaryexpression ^^
      { case primaryexpression ~ "=" ~ rvalue => RelationalExpressionEq(primaryexpression, rvalue) }
    | primaryexpression ~ "!=" ~ primaryexpression ^^
      { case primaryexpression ~ "!=" ~ rvalue => RelationalExpressionNe(primaryexpression, rvalue) }
    | primaryexpression ~ "<" ~ primaryexpression ^^
      { case primaryexpression ~ "<" ~ rvalue => RelationalExpressionLt(primaryexpression, rvalue) }
    | primaryexpression ~ "IS" ~ "NULL" ^^
      { case primaryexpression ~ "IS" ~ "NULL" => RelationalExpressionNull(primaryexpression) }
    | primaryexpression ~ "IS" ~ "NOT" ~ "NULL" ^^
      { case primaryexpression ~ "IS" ~ "NOT" ~ "NULL" => RelationalExpressionNotNull(primaryexpression) }
    | primaryexpression ^^
      { case primaryexpression => primaryexpression }
  )

  def primaryexpression:Parser[Expression] = (
      fqattribute ^^ { PrimaryExpressionAttr(_) }
    | int ^^ { i => PrimaryExpressionTyped(Datatype.INTEGER, Name(i)) }
    | chars  ^^ { x => PrimaryExpressionTyped(Datatype.STRING, Name(x.substring(1, x.size - 1))) }
    | "NULL" ^^ { case "NULL" => ConstNULL() }
    | "CONCAT" ~ "(" ~ rep1sep(expression, ",") ~ ")" ^^ { case "CONCAT"~"("~expressions~")" => Concat(expressions) }
    | "(" ~ expression ~ ")" ^^ { case "("~x~")" => x }
  )

}

