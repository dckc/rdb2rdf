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
case class TriplesBlock(triplepatterns:List[TriplePattern]) extends GraphPattern
case class EmptyGraphPattern() extends GraphPattern
case class TableConjunction(gps:List[GraphPattern]) extends GraphPattern
case class TableDisjunction(gps:List[GraphPattern]) extends GraphPattern
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
    {
      case "{"~tbOPT~gpntORf_tbOPT~"}" => {
	var init = tbOPT match {
	  case Some(x) => x
	  case _ => EmptyGraphPattern()
	}
	gpntORf_tbOPT.foldLeft(init)((gp, lentry) => lentry match {
	  case ~(TableFilter(null, expr), None) => TableFilter(gp, expr)
	  case ~(TriplesBlock(triples), None) => gp match {
	    case EmptyGraphPattern() => TriplesBlock(triples)
	    case TriplesBlock(triples2) => TableConjunction(List(gp, TriplesBlock(triples2)))
	  }
	  case ~(TableDisjunction(list), None) => gp match {
	    case EmptyGraphPattern() => TableDisjunction(list)
	    case x => TableConjunction(List(gp, x))
	  }
	  case ~(OptionalGraphPattern(gp2), None) => TableConjunction(List(gp, OptionalGraphPattern(gp2)))
	  case x => error("found " + x)
	})
      }
    }
  )

  def graphpatternnottriplesORfilter:Parser[GraphPattern] = (
      "OPTIONAL"~groupgraphpattern ^^ { case "OPTIONAL"~ggp => OptionalGraphPattern(ggp) }
    | rep1sep(groupgraphpattern, "UNION") ^^ { x => if (x.size > 1) TableDisjunction(x) else x(0) }
    | "GRAPH"~uri~groupgraphpattern ^^ { case "GRAPH"~u~ggp => GraphGraphPattern(ggp) }
    | filter ^^ { x => TableFilter(null, x) }
  )

  def triplesblock:Parser[TriplesBlock] =
    rep1sep(triplepattern, ".") ^^ { case pats => TriplesBlock(pats) }

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
