package w3c.sw

import scala.util.parsing.combinator._
import java.net.URI

case class Arith() extends JavaTokenParsers {
  def expr: Parser[Any] = term~rep("+"~term | "-"~term)
  def term: Parser[Any] = factor~rep("*"~factor | "/"~factor)
  def factor: Parser[Any] = floatingPointNumber | "("~expr~")"
}

object MyParsers extends RegexParsers {

  val uri = """[a-zA-Z0-9:]+""".r
  val varr = """\?[a-zA-Z0-9]+""".r

}

import MyParsers._

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

sealed abstract class Lit
case class LitInt(i:Int) extends Lit
case class LitString(s:String) extends Lit

case class Stem(s:String)
case class Attr(s:String)
case class Rel(s:Rel)

case class Var(s:String)

case class Sparql() extends JavaTokenParsers {

  def triplepatterns = repsep(triplepattern, ".") 
  def triplepattern = subject ~ predicate ~ objectt
  def subject = uri | varr
  def predicate = uri
  def objectt = uri | varr | literal
  def literal = stringLiteral

}

