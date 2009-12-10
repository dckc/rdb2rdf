package w3c.sw

import java.net.URI

case class RDFTriple(s:RDFSubject, p:RDFPredicate, o:RDFObject)

sealed abstract class RDFSubject()
case class RDFSubjectUri(uri:URI) extends RDFSubject
case class RDFSubjectBlankNode(b:BlankNode) extends RDFSubject

case class RDFPredicate(uri:URI)

sealed abstract class RDFObject()
case class RDFObjectUri(uri:URI) extends RDFObject
case class RDFObjectBlankNode(b:BlankNode) extends RDFObject

case class BlankNode(debugName:String)

case class RDFLiteral(lexicalForm:String, datatype:Datatype)
case class Datatype(uri:URI)

object RDFLiteral {
  val StringDatatype = Datatype(new URI("http://www.w3.org/2001/XMLSchema#string"))
  val IntegerDatatype = Datatype(new URI("http://www.w3.org/2001/XMLSchema#integer"))
}
