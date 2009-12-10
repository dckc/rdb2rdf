package w3c.sw

import scala.util.parsing.combinator._
import java.net.URI

object MyParsers extends RegexParsers {

  val uri = """[a-zA-Z0-9:/#\.]+""".r
  val name = """[a-zA-Z_][a-zA-Z0-9_]*""".r
}

import MyParsers._

// case class RDFTriple()

// sealed abstract class RDFSubject()
// case class RDFSubjectUri(uri:URI) extends RDFSubject
// case class RDFSubjectBlankNode(debugName:String) extends RDFSubject

// case class RDFPredicate(uri:URI)

// sealed abstract class RDFObject()
// case class RDFObjectUri(uri:URI) extends RDFObject
// case class RDFObjectBlankNode(debugName:String) extends RDFObject
// case class RDFLiteral(lexicalForm:Literal, datatype:URI)

// case class RelationStemGraph(m:Map[String,String])
// case class RelationMap(Map[(FQAttribute,Tuple),RDFTriple])

// case class Tuple()
// case class StemURI()
// case class NodeMap()
// case class PredicateMap()
// case class LiteralMap()
