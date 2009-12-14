package w3c.sw

import scala.util.parsing.combinator._
import java.net.URI

import MyParsers._

case class Select(attributelist:AttributeList, tablelist:TableList, expression:Option[Expression])
case class AttributeList(attributes:List[NamedAttribute])
case class NamedAttribute(fqattribute:AliasAttribute, attribute:Attribute)
//case class RelAttribute(relation:Relation, attribute:Attribute)
case class AliasAttribute(alias:Alias, attribute:Attribute)
case class Attribute(n:Name)
case class Relation(n:Name)
case class Alias(n:Name)
case class TableList(joins:List[Join])
case class Join(relasalias:RelAsAlias, expression:Option[Expression])
case class RelAsAlias(rel:Relation, as:Alias)
case class Expression(conjuncts:List[PrimaryExpression])
sealed abstract class PrimaryExpression
case class PrimaryExpressionEq(l:AliasAttribute, r:RValue) extends PrimaryExpression
case class PrimaryExpressionLt(l:AliasAttribute, r:RValue) extends PrimaryExpression
case class PrimaryExpressionNotNull(l:AliasAttribute) extends PrimaryExpression
sealed abstract class RValue
case class RValueAttr(fqattribute:AliasAttribute) extends RValue
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
    { case "SELECT" ~ attributes ~ "FROM" ~ tables ~ whereexpr =>
      Select(attributes, tables, whereexpr) }

  def where:Parser[Expression] =
    "WHERE" ~ expression ^^ { case "WHERE" ~ expression => expression }

  def attributelist:Parser[AttributeList] =
    repsep(namedattribute, ",") ^^ { AttributeList(_) }

  def namedattribute:Parser[NamedAttribute] =
    fqattribute ~ "AS" ~ attribute ^^
    { case fqattribute ~ "AS" ~ attribute =>
      NamedAttribute(fqattribute, attribute) }

  def fqattribute:Parser[AliasAttribute] =
    alias ~ "." ~ attribute ^^
    { case alias ~ "." ~ attribute => AliasAttribute(alias, attribute) }

  def attribute:Parser[Attribute] =
    """[a-zA-Z_]\w*""".r ^^ { x => Attribute(Name(x)) }

  def relation:Parser[Relation] =
    """[a-zA-Z_]\w*""".r ^^ { x => Relation(Name(x)) }

  def alias:Parser[Alias] =
    """[a-zA-Z_]\w*""".r ^^ { x => Alias(Name(x)) }

  def tablelist:Parser[TableList] =
    repsep(join, "INNER" ~ "JOIN") ^^ { TableList(_) }

  def join:Parser[Join] =
    relasalias ~ opt(optexpr) ^^
    { case relasalias ~ optexpr => Join(relasalias, optexpr) }

  def optexpr:Parser[Expression] =
    "ON" ~ expression ^^ { case "ON"~expression => expression }

  def relasalias:Parser[RelAsAlias] =
    relation ~ "AS" ~ alias ^^
    { case rel1 ~ "AS" ~ rel2 => RelAsAlias(rel1, rel2) }

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

