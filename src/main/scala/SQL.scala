package w3c.sw

import scala.util.parsing.combinator._
import java.net.URI

case class Select(attributelist:AttributeList, tablelist:TableList, expression:Expression)
case class AttributeList(attributes:List[NamedAttribute])
case class NamedAttribute(fqattribute:RelAliasAttribute, attralias:AttrAlias)
//case class RelAttribute(relation:Relation, attribute:Attribute) c.f. ForeignKey
case class RelAliasAttribute(relalias:RelAlias, attribute:Attribute)
case class Attribute(n:Name)
case class AttrAlias(n:Name)
case class Relation(n:Name)
case class RelAlias(n:Name)
case class TableList(joins:List[AliasedResource])
case class AliasedResource(rel:Relation, as:RelAlias)
case class Expression(conjuncts:List[PrimaryExpression])
sealed abstract class PrimaryExpression
case class PrimaryExpressionEq(l:RelAliasAttribute, r:RValue) extends PrimaryExpression
case class PrimaryExpressionLt(l:RelAliasAttribute, r:RValue) extends PrimaryExpression
case class PrimaryExpressionNotNull(l:RelAliasAttribute) extends PrimaryExpression
sealed abstract class RValue
case class RValueAttr(fqattribute:RelAliasAttribute) extends RValue
case class RValueTyped(datatype:SQLDatatype, i:Name) extends RValue
case class Name(s:String)

object Name {
  implicit def fromStringToName(s:String):Name = Name(s)
}

case class SQLDatatype(name:String)

object SQLDatatype {
  val STRING = SQLDatatype("String")
  val INTEGER = SQLDatatype("Int")
}

sealed abstract class ValueDescription
case class Value(datatype:SQLDatatype) extends ValueDescription
case class ForeignKey(rel:Relation, attr:Attribute) extends ValueDescription

case class DatabaseDesc(relationdescs:Map[Relation,RelationDesc])
case class RelationDesc(primarykey:Attribute, attributes:Map[Attribute, ValueDescription])

case class Sql() extends JavaTokenParsers {

  def select:Parser[Select] =
    "SELECT" ~ attributelist ~ "FROM" ~ tablelist ~ opt(where) ^^
    {
      case "SELECT" ~ attributes ~ "FROM" ~ tables ~ whereexpr => {
	val expression:Expression = whereexpr match {
	  case None => Expression(List())
	  case Some(f) => f
	}
	Select(attributes, tables, expression)
      }
    }

  def where:Parser[Expression] =
    "WHERE" ~ expression ^^ { case "WHERE" ~ expression => expression }

  def attributelist:Parser[AttributeList] =
    repsep(namedattribute, ",") ^^ { AttributeList(_) }

  def namedattribute:Parser[NamedAttribute] =
    fqattribute ~ "AS" ~ attralias ^^
    { case fqattribute ~ "AS" ~ attralias =>
      NamedAttribute(fqattribute, attralias) }

  def fqattribute:Parser[RelAliasAttribute] =
    relalias ~ "." ~ attribute ^^
    { case relalias ~ "." ~ attribute => RelAliasAttribute(relalias, attribute) }

  def attribute:Parser[Attribute] =
    """[a-zA-Z_]\w*""".r ^^ { x => Attribute(Name(x)) }

  def attralias:Parser[AttrAlias] =
    """[a-zA-Z_]\w*""".r ^^ { x => AttrAlias(Name(x)) }

  def relation:Parser[Relation] =
    """[a-zA-Z_]\w*""".r ^^ { x => Relation(Name(x)) }

  def relalias:Parser[RelAlias] =
    """[a-zA-Z_]\w*""".r ^^ { x => RelAlias(Name(x)) }

  def tablelist:Parser[TableList] =
    repsep(aliasedjoin, "INNER" ~ "JOIN") ^^ { TableList(_) }

  def aliasedjoin:Parser[AliasedResource] =
    relation ~ "AS" ~ relalias ^^
    { case rel1 ~ "AS" ~ rel2 => AliasedResource(rel1, rel2) }

  def expression:Parser[Expression] = 
    repsep(primaryexpression, "AND") ^^ 
    { Expression(_) }

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

