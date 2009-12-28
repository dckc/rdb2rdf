package w3c.sw

import scala.util.parsing.combinator._
import java.net.URI

object MyParsers extends RegexParsers {

  val uri = """[a-zA-Z0-9:/#\.]+""".r
  val name = """[a-zA-Z_][a-zA-Z0-9_]*""".r
}

import MyParsers._

case class SparqlSelect(attrs:SparqlAttributeList, gp:GraphPattern)
case class SparqlAttributeList(attributelist:List[Var])

sealed abstract class GraphPattern
case class TriplesBlock(triplepatterns:List[TriplePattern], filter:SparqlExpression) extends GraphPattern
case class TableConjunction(gps:List[GraphPattern]) extends GraphPattern
case class TableDisjunction(gps:List[GraphPattern]) extends GraphPattern
case class ParserTableFilter(expr:SparqlExpression) extends GraphPattern
case class TableFilter(gp:GraphPattern, expr:SparqlExpression) extends GraphPattern
case class OptionalGraphPattern(gp:GraphPattern) extends GraphPattern
case class GraphGraphPattern(gp:GraphPattern) extends GraphPattern

case class TriplePattern(s:S, p:P, o:O)
case class ObjUri(stem:Stem, rel:Rel, attr:Attr, v:CellValue)
case class CellValue(s:String)

sealed abstract class S
case class SUri(obj:ObjUri) extends S
case class SVar(v:Var) extends S

sealed abstract class O
case class OUri(obj:ObjUri) extends O
case class OVar(v:Var) extends O
case class OLit(lit:SparqlLiteral) extends O

sealed abstract class P
case class PUri(stem:Stem, rel:Rel, attr:Attr) extends P
case class PVar(v:Var) extends P

case class SparqlLiteral(lit:RDFLiteral)

case class Stem(s:String)
case class Attr(s:String)
case class Rel(s:String)

case class Var(s:String)

case class SparqlExpression(conjuncts:List[SparqlPrimaryExpression])
sealed abstract class SparqlPrimaryExpression
case class SparqlPrimaryExpressionEq(left:SparqlTermExpression, right:SparqlTermExpression) extends SparqlPrimaryExpression
case class SparqlPrimaryExpressionLt(left:SparqlTermExpression, right:SparqlTermExpression) extends SparqlPrimaryExpression
case class SparqlTermExpression(term:Term)

sealed abstract class Term
case class TermUri(obj:ObjUri) extends Term
case class TermVar(v:Var) extends Term
case class TermLit(lit:SparqlLiteral) extends Term


case class Sparql() extends JavaTokenParsers {

  def select:Parser[SparqlSelect] =
    "SELECT" ~ attributelist ~ groupgraphpattern ^^ { case "SELECT"~a~gp => SparqlSelect(a, gp) }

  def filter:Parser[SparqlExpression] =
    "FILTER" ~ "(" ~ expression ~ ")" ^^ { case "FILTER"~"("~expression~")" => expression }

  def expression:Parser[SparqlExpression] = 
    repsep(primaryexpression, "&&") ^^ 
    { SparqlExpression(_) }

  def primaryexpression:Parser[SparqlPrimaryExpression] = (
      value ~ "=" ~ value ^^
      { case left ~ "=" ~ right => SparqlPrimaryExpressionEq(left, right) }
    | value ~ "<" ~ value ^^
      { case left ~ "<" ~ right => SparqlPrimaryExpressionLt(left, right) }
  )

  def value:Parser[SparqlTermExpression] = (
      "<"~uri~">" ^^ { case "<"~x~">" => SparqlTermExpression(TermUri(Sparql.parseObjectURI(x))) }
    | varr ^^ { x => SparqlTermExpression(TermVar(x)) }
    | literal ^^ { x => SparqlTermExpression(TermLit(x)) }
  )

  def attributelist:Parser[SparqlAttributeList] =
    rep(varr) ^^ { SparqlAttributeList(_) }

  def groupgraphpattern:Parser[GraphPattern] = (
    "{" ~ opt(triplesblock) ~ rep(graphpatternnottriplesORfilter ~ opt(triplesblock)) ~ "}" ^^
    //"{" ~ opt(triplesblock) ~ rep(graphpatternnottriplesORfilter_OPTtriplesblock) ~ "}" ^^
    {
      case "{"~tbOPT~gpntORf_tbOPT~"}" => {
	val l:Option[GraphPattern] = tbOPT
	val r:List[~[GraphPattern,Option[TriplesBlock]]] = gpntORf_tbOPT
	(tbOPT, gpntORf_tbOPT) match {
	  case (Some(x), list) => {
	    if (list.size == 0)
	      x
	    else {
	      println("ignoring " + list)
	      TableConjunction(List[GraphPattern](x))
	    }
	  }
	}
      }
    }
  )

  def graphpatternnottriplesORfilter_OPTtriplesblock:Parser[(GraphPattern, Option[TriplesBlock])] =
    graphpatternnottriplesORfilter ~ opt(triplesblock) ^^ { case a~b => (a, b) }

  def graphpatternnottriplesORfilter:Parser[GraphPattern] = (
      "OPTIONAL"~groupgraphpattern ^^ { case "OPTIONAL"~ggp => OptionalGraphPattern(ggp) }
    | rep1sep(groupgraphpattern, "UNION") ^^ { x => if (x.size > 1) TableDisjunction(x) else x(0) }
    | "GRAPH"~uri~groupgraphpattern ^^ { case "GRAPH"~u~ggp => GraphGraphPattern(ggp) }
    | filter ^^ { x => ParserTableFilter(x) }
  )

  def triplesblock:Parser[TriplesBlock] =
    repsep(triplepattern, ".") ~ opt(filter) ^^ {
      case pats~filter =>
	val sparqlExpression:SparqlExpression = filter match {
	  case None => SparqlExpression(List())
	  case Some(f) => f
	}
      TriplesBlock(pats, sparqlExpression)
    }

  def triplepattern:Parser[TriplePattern] =
    subject ~ predicate ~ objectt ^^ { case s~p~o => TriplePattern(s, p, o) }

  def subject:Parser[S] = (
      "<"~uri~">" ^^ { case "<"~x~">" => SUri(Sparql.parseObjectURI(x)) }
    | varr ^^ { x => SVar(x) }
  )

  def predicate:Parser[P] =
    "<"~uri~">" ^^ { case "<"~x~">" => Sparql.parsePredicateURI(x) }

  def objectt:Parser[O] = (
      "<"~uri~">" ^^ { case "<"~x~">" => OUri(Sparql.parseObjectURI(x)) }
    | varr ^^ { x => OVar(x) }
    | literal ^^ { x => OLit(x) }
  )

  def literal:Parser[SparqlLiteral] = (
      stringLiteral~"^^<http://www.w3.org/2001/XMLSchema#string>" ^^
      { case lit ~ _ => SparqlLiteral(RDFLiteral(lit.substring(1,lit.size - 1), RDFLiteral.StringDatatype)) }
    | stringLiteral~"^^<http://www.w3.org/2001/XMLSchema#integer>" ^^
      { case lit ~ _ => SparqlLiteral(RDFLiteral(lit.substring(1,lit.size - 1), RDFLiteral.IntegerDatatype)) }
)

  def varr:Parser[Var] = "?"~ident ^^ { case "?"~x => Var(x) }

}

object Sparql {

  /* stemURI + '/' + (\w+) + '#' (\w+) */
  def parsePredicateURI(x:String):PUri = {
    val uri = new URI(x)
    val path = uri.getPath().split("/").toList.filterNot(_ == "")
    val subPath = path.slice(0, path.size - 1).mkString("/")
    val stem = uri.getScheme() + "://" + uri.getAuthority + "/" + subPath
    PUri(Stem(stem), Rel(path.last), Attr(uri.getFragment))
  }

  /* stemURI + '/' (\w+) '/' (\w+) '.' (\w+) '#record' */
  def parseObjectURI(x:String):ObjUri = {
    val uri = new URI(x)
    val path = uri.getPath().split("/").toList.filterNot(_ == "")
    val subPath = path.slice(0, path.size - 2).mkString("/")
    val rel = path(path.size - 2)
    val attrPair = path(path.size-1).split("\\.")
    val stem = uri.getScheme() + "://" + uri.getAuthority + "/" + subPath
    assert("record" == uri.getFragment)
    ObjUri(Stem(stem), Rel(rel), Attr(attrPair(0)), CellValue(attrPair(1)))
  }

}
