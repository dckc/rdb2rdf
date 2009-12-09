package w3c.sw

import scala.util.parsing.combinator._
import java.net.URI

import MyParsers._

case class Select(attributelist:AttributeList, tablelist:TableList, expression:Expression)

case class AttributeList(attributes:List[NamedAttribute])
case class NamedAttribute(fqattribute:FQAttribute, attribute:Attribute)
case class FQAttribute(relation:Relation, attribute:Attribute)
case class Attribute(n:Name)
case class Relation(n:Name)
case class TableList(tablealias:TableAlias, joins:List[Join])
case class Join(tablealias:TableAlias, expression:Expression)
case class TableAlias(rel:Relation, as:Relation)
case class Expression(conjuncts:List[PrimaryExpression])
sealed abstract class PrimaryExpression
case class PrimaryExpressionEq(l:FQAttribute, r:RValue)
case class PrimaryExpressionLt(l:FQAttribute, r:RValue)
case class PrimaryExpressionNotNull(l:FQAttribute)
sealed abstract class RValue
case class RValueAttr(fqattribute:FQAttribute)
case class RValueInt(i:Name)
case class RValueString(i:Name)

case class Name(s:String)


case class Sql() extends JavaTokenParsers {

  def select = "SELECT" ~ attributelist ~ "FROM" ~ tablelist ~ opt("WHERE" ~ expression)
  def attributelist = repsep(namedattribute, ",")
  def namedattribute = fqattribute ~ "AS" ~ attribute
  def fqattribute = relation ~ "." ~ attribute
  def attribute = """[a-zA-Z_]\w*""".r
  def relation = """[a-zA-Z_]\w*""".r
  def tablelist = tablealias ~ rep("INNER" ~ "JOIN" ~ tablealias ~ "ON" ~ expression)
  def tablealias = relation ~ "AS" ~ relation
  def expression = repsep(primaryexpression, "AND")
  def primaryexpression = (  fqattribute ~ "=" ~ rvalue
			   | fqattribute ~ "<" ~ rvalue
			   | fqattribute ~ "IS" ~ "NOT" ~ "NULL" )
  def rvalue = (  fqattribute
		| """[0-9]+""".r
		| "\"[^\"]*\"".r )

}

