package w3c.sw

import scala.util.parsing.combinator._
import java.net.URI

object MyParsers extends RegexParsers {

  val uri = """[a-zA-Z0-9:/#\.]+""".r
  val name = """[a-zA-Z_][a-zA-Z0-9_]*""".r
}

import MyParsers._


// case class RelationStemGraph(m:Map[String,String])
// case class RelationMap(Map[(FQAttribute,Tuple),RDFTriple])

// case class Tuple()
// case class StemURI()
// case class NodeMap()
// case class PredicateMap()
// case class LiteralMap()
