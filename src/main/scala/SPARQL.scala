package w3c.sw.sparql

import w3c.sw.rdf._
import scala.util.parsing.combinator._
import java.net.URI

object MyParsers extends RegexParsers {

  val uri = """[a-zA-Z0-9:/#\.]+""".r
  val name = """[a-zA-Z_][a-zA-Z0-9_]*""".r
  var prefixes:Map[String, String] = Map()
}

import MyParsers._

case class Select(attrs:SparqlAttributeList, gp:GraphPattern)
case class SparqlAttributeList(attributelist:List[Var])

sealed abstract class GraphPattern
case class TriplesBlock(triplepatterns:List[TriplePattern]) extends GraphPattern
case class TableConjunction(gps:List[GraphPattern]) extends GraphPattern {
  assert (!(gps exists (x => { x match { case TableConjunction(_) => true case _ => false } })))
}
case class TableDisjunction(gps:List[GraphPattern]) extends GraphPattern
case class TableFilter(gp:GraphPattern, expr:Expression) extends GraphPattern
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
case class OLit(lit:Literal) extends O

sealed abstract class P
case class PUri(stem:Stem, rel:Rel, attr:Attr) extends P
case class PVar(v:Var) extends P

case class Literal(lit:RDFLiteral)

case class Stem(s:String)
case class Attr(s:String)
case class Rel(s:String)

case class Var(s:String)

case class Expression(conjuncts:List[PrimaryExpression])
sealed abstract class PrimaryExpression
case class PrimaryExpressionEq(left:SparqlTermExpression, right:SparqlTermExpression) extends PrimaryExpression
case class PrimaryExpressionLt(left:SparqlTermExpression, right:SparqlTermExpression) extends PrimaryExpression
case class SparqlTermExpression(term:Term)

sealed abstract class Term
case class TermUri(obj:ObjUri) extends Term
case class TermVar(v:Var) extends Term
case class TermLit(lit:Literal) extends Term


case class Sparql() extends JavaTokenParsers {

  def select:Parser[Select] =
    rep(prefixdecls) ~ "SELECT" ~ attributelist ~ opt("WHERE") ~ groupgraphpattern ^^ { case x~"SELECT"~a~w~gp => Select(a, gp) }

  def prefixdecls:Parser[Unit] =
    "PREFIX" ~ name ~ ":" ~ qnameORuri ^^ { case "PREFIX"~pre~":"~u => prefixes += (pre -> u) }

  def filter:Parser[Expression] =
    "FILTER" ~ "(" ~ expression ~ ")" ^^ { case "FILTER"~"("~expression~")" => expression }

  def expression:Parser[Expression] = 
    repsep(primaryexpression, "&&") ^^ 
    { Expression(_) }

  def primaryexpression:Parser[PrimaryExpression] = (
      value ~ "=" ~ value ^^
      { case left ~ "=" ~ right => PrimaryExpressionEq(left, right) }
    | value ~ "<" ~ value ^^
      { case left ~ "<" ~ right => PrimaryExpressionLt(left, right) }
  )

  def value:Parser[SparqlTermExpression] = (
      qnameORuri ^^ { case x => SparqlTermExpression(TermUri(Sparql.parseObjectURI(x))) }
    | varr ^^ { x => SparqlTermExpression(TermVar(x)) }
    | literal ^^ { x => SparqlTermExpression(TermLit(x)) }
  )

  def attributelist:Parser[SparqlAttributeList] =
    rep(varr) ^^ { SparqlAttributeList(_) }

  def groupgraphpattern:Parser[GraphPattern] = (
    "{" ~ opt(triplesblock) ~ rep(graphpatternnottriplesORfilter ~ opt(triplesblock)) ~ "}" ^^
    {
      case "{"~tbOPT~gpntORf_tbOPT~"}" => {

// case class TriplesBlock(triplepatterns:List[TriplePattern]) extends GraphPattern
// case class TableConjunction(gps:List[GraphPattern]) extends GraphPattern
// case class TableDisjunction(gps:List[GraphPattern]) extends GraphPattern
// case class TableFilter(gp:GraphPattern, expr:Expression) extends GraphPattern
// case class OptionalGraphPattern(gp:GraphPattern) extends GraphPattern
// case class GraphGraphPattern(gp:GraphPattern) extends GraphPattern

	// println("groupgraphpattern(" + tbOPT + ", " + gpntORf_tbOPT + ")")
	val init:Option[GraphPattern] = tbOPT
	gpntORf_tbOPT.foldLeft(init)((gp, lentry) => {//println("match: " + (gp, lentry))
	  // print("case (" + gp + ", " + lentry + ")")
	  (gp, lentry) match {
	    case (Some(TableFilter(TriplesBlock(l), Expression(lexp))), ~(TableFilter(null, Expression(expr)), Some(TriplesBlock(r)))) => Some(TableFilter(TriplesBlock(l ++ r), Expression(lexp ++ expr)))
	    case (Some(TriplesBlock(l)), ~(TableFilter(null, expr), Some(TriplesBlock(r)))) => Some(TableFilter(TriplesBlock(l ++ r), expr))
	    case (Some(gp             ), ~(TableFilter(null, expr), None                 )) => Some(TableFilter(gp, expr))
	    case (None,                  ~(TableFilter(null, expr), Some(TriplesBlock(r)))) => Some(TableFilter(TriplesBlock(r), expr))

	    // case (None,     ~(TableConjunction(gps), None    )) => TableConjunction(gps)
	    // case (Some(gp), ~(TableConjunction(gps), None    )) => TableConjunction(List(List(gp) ++ gps))
	    // case (None,     ~(TableConjunction(gps), Some(tb))) => TableConjunction(List(gps ++ List(tb)))
	    // case (Some(gp), ~(TableConjunction(gps), Some(tb))) => TableConjunction(List(List(gp) ++ gps ++ List(tb)))

	    case (None                    ,  ~(x, None    )) => Some(x                                     )
	    case (Some(TableConjunction(l)), ~(x, None    )) => Some(TableConjunction(l ++ List(    x    )))
	    case (Some(gp                 ), ~(x, None    )) => Some(TableConjunction(     List(gp, x    )))
	    case (None                     , ~(x, Some(tb))) => Some(TableConjunction(     List(    x, tb)))
	    case (Some(TableConjunction(l)), ~(x, Some(tb))) => Some(TableConjunction(l ++ List(    x, tb)))
	    case (Some(gp                 ), ~(x, Some(tb))) => Some(TableConjunction(     List(gp, x, tb)))

	    case x => error("found " + x)
	  }
	}).get
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
      qnameORuri ^^ { case x => SUri(Sparql.parseObjectURI(x)) }
    | varr ^^ { x => SVar(x) }
  )

  def predicate:Parser[P] =
    qnameORuri ^^ { case x => Sparql.parsePredicateURI(x) }

  def objectt:Parser[O] = (
      qnameORuri ^^ { case x => OUri(Sparql.parseObjectURI(x)) }
    | varr ^^ { x => OVar(x) }
    | literal ^^ { x => OLit(x) }
  )

  def qnameORuri:Parser[String] = (
      "<"~uri~">" ^^ { case "<"~x~">" => x }
    | name~":"~name ^^ { case prefix~":"~localName => prefixes(prefix) + localName }
  )

  def literal:Parser[Literal] =
      stringLiteral~"^^"~qnameORuri ^^
      {
	case lit~"^^"~dt => Literal(RDFLiteral(lit.substring(1,lit.size - 1), dt match {
	  case "http://www.w3.org/2001/XMLSchema#string" => RDFLiteral.StringDatatype
	  case "http://www.w3.org/2001/XMLSchema#integer" => RDFLiteral.IntegerDatatype
	  case "http://www.w3.org/2001/XMLSchema#date" => RDFLiteral.DateDatatype
	  // case "http://www.w3.org/2001/XMLSchema#dateTime" => RDFLiteral.DateTimeDatatype
	  case x => error("only programed to deal with string and integer, not " + x)
	}))
      }

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
