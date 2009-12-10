package w3c.sw

import scala.util.parsing.combinator._
import java.net.URI

import MyParsers._

case class SparqlSelect(attrs:SparqlAttributeList, triples:TriplePatterns)
case class SparqlAttributeList(attributelist:List[Var])

case class TriplePatterns(triplepatterns:List[TriplePattern])
case class TriplePattern(s:S, p:P, o:O)

sealed abstract class S
case class SUri(uri:URI) extends S
case class SVar(v:Var) extends S

sealed abstract class O
case class OUri(uri:URI) extends O
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

case class Sparql() extends JavaTokenParsers {

  def select:Parser[SparqlSelect] =
    "SELECT" ~ attributelist ~ "{" ~ triplepatterns ~ "}" ^^ { case "SELECT"~a~"{"~t~"}" => SparqlSelect(a, t) }

  def attributelist:Parser[SparqlAttributeList] =
    rep(varr) ^^ { SparqlAttributeList(_) }

  def triplepatterns:Parser[TriplePatterns] =
    repsep(triplepattern, ".") ^^ { TriplePatterns(_) }

  def triplepattern:Parser[TriplePattern] =
    subject ~ predicate ~ objectt ^^ { case s~p~o => TriplePattern(s, p, o) }

  def subject:Parser[S] = (
      uri ^^ { x => SUri(new URI(x)) }
    | varr ^^ { x => SVar(x) }
  )

  def predicate:Parser[P] =
    "<"~uri~">" ^^ { case "<"~x~">" => Sparql.parsePredicateURI(x) }

  def objectt:Parser[O] = (
      uri ^^ { x => OUri(new URI(x)) }
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

  def parsePredicateURI(x:String):PUri = {
    val uri = new URI(x)
    val path = uri.getPath().split("/")
    val subPath = path.toList.remove(_ == "").slice(0, path.size - 2).mkString("/")
    val stem = uri.getScheme() + "://" + uri.getAuthority + "/" + subPath
    PUri(Stem(stem), Rel(path.last), Attr(uri.getFragment))
  }

}
