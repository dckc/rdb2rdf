package w3c.sw

import scala.util.parsing.combinator._
import java.net.URI

import MyParsers._

// object SparqlTypeAliases {
//   type TriplePatterns = List[TriplePattern]
// }

// import SpqrqlTypeAliases._

case class TriplePatterns(triplepatterns:List[TriplePattern])
case class TriplePattern(s:S, p:P, o:O)

sealed abstract class S
case class SUri(uri:URI) extends S
case class SVar(v:Var) extends S

sealed abstract class O
case class OUri(uri:URI) extends O
case class OVar(v:Var) extends O
case class OLit(lit:Lit) extends O

sealed abstract class P
case class PUri(stem:Stem, rel:Rel, attr:Attr) extends P
case class PVar(v:Var) extends P

// sealed abstract class Lit
// case class LitInt(i:Int) extends Lit
// case class LitString(s:String) extends Lit
case class Lit(lexicalForm:String, datatype:Datatype)
case class Datatype(uri:URI)

object Lit {
  val StringDatatype = Datatype(new URI("http://www.w3.org/2001/XMLSchema#string"))
  val IntegerDatatype = Datatype(new URI("http://www.w3.org/2001/XMLSchema#integer"))
}

case class Stem(s:String)
case class Attr(s:String)
case class Rel(s:String)

case class Var(s:String)

case class Sparql() extends JavaTokenParsers {

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

// case class Lit(lexicalForm:String, datatype:Datatype)
// case class Datatype(uri:URI)

  def literal:Parser[Lit] = (
      stringLiteral~"^^<http://www.w3.org/2001/XMLSchema#string>" ^^
      { case lit ~ _ => Lit(lit.substring(1,lit.size - 1), Lit.StringDatatype) }
    | stringLiteral~"^^<http://www.w3.org/2001/XMLSchema#integer>" ^^
      { case lit ~ _ => Lit(lit.substring(1,lit.size - 1), Lit.IntegerDatatype) }
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
