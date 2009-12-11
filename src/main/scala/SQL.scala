package w3c.sw

import scala.util.parsing.combinator._
import java.net.URI

import MyParsers._

case class Select(attributelist:AttributeList, tablelist:TableList, expression:Option[Expression])
case class AttributeList(attributes:List[NamedAttribute])
case class NamedAttribute(fqattribute:FQAttribute, attribute:Attribute)
case class FQAttribute(relation:Relation, attribute:Attribute)
case class Attribute(n:Name)
case class Relation(n:Name)
case class TableList(tablealias:TableAlias, joins:List[Join])
case class Join(tablealias:TableAlias, expression:Option[Expression])
case class TableAlias(rel:Relation, as:Relation)
case class Expression(conjuncts:List[PrimaryExpression])
sealed abstract class PrimaryExpression
case class PrimaryExpressionEq(l:FQAttribute, r:RValue) extends PrimaryExpression
case class PrimaryExpressionLt(l:FQAttribute, r:RValue) extends PrimaryExpression
case class PrimaryExpressionNotNull(l:FQAttribute) extends PrimaryExpression
sealed abstract class RValue
case class RValueAttr(fqattribute:FQAttribute) extends RValue
case class RValueInt(i:Name) extends RValue
case class RValueString(i:Name) extends RValue
case class Name(s:String)

case class Sql() extends JavaTokenParsers {

  def select:Parser[Select] =
    "SELECT" ~ attributelist ~ "FROM" ~ tablelist ~ opt(where) ^^
    { case "SELECT" ~ attributes ~ "FROM" ~ tables ~ whereexpr =>
      Select(attributes, tables, whereexpr) }

  def where:Parser[Expression] =
    "WHERE" ~ expression ^^ { case "WHERE" ~ expression => expression }

  def attributelist:Parser[AttributeList] =
    repsep(namedattribute, ",") ^^ { AttributeList(_)}

  def namedattribute:Parser[NamedAttribute] =
    fqattribute ~ "AS" ~ attribute ^^
    { case fqattribute ~ "AS" ~ attribute =>
      NamedAttribute(fqattribute, attribute) }

  def fqattribute:Parser[FQAttribute] =
    relation ~ "." ~ attribute ^^
    { case relation ~ "." ~ attribute => FQAttribute(relation, attribute) }

  def attribute:Parser[Attribute] =
    """[a-zA-Z_]\w*""".r ^^ { x => Attribute(Name(x)) }

  def relation:Parser[Relation] =
    """[a-zA-Z_]\w*""".r ^^ { x => Relation(Name(x)) }

  def tablelist:Parser[TableList] =
    tablealias ~ rep("INNER" ~ "JOIN" ~ tablealias ~ opt(optexpr)) ^^
    { case tablealias ~ repjoins =>
        val joins:List[Join] = repjoins map { case "INNER"~"JOIN"~tablealias~optexpr => Join(tablealias, optexpr) }
        TableList(tablealias, joins)
    }

  def optexpr:Parser[Expression] =
    "ON" ~ expression ^^ { case "ON"~expression => expression }

  def tablealias:Parser[TableAlias] =
    relation ~ "AS" ~ relation ^^
    { case rel1 ~ "AS" ~ rel2 => TableAlias(rel1, rel2) }

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
    | """[0-9]+""".r ^^ { x => RValueInt(Name(x)) }
    | "\"[^\"]*\"".r  ^^ { x => RValueString(Name(x.substring(1, x.size - 1))) }
  )

}

