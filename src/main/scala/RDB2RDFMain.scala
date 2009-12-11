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
case class StemURI(s:String)
case class PrimaryKey(attr:Attribute)
// case class NodeMap()
// case class PredicateMap()
// case class LiteralMap()

/**
 * Traverses nested BGP.
 * VARconstraint(VAR, FQAttribute):
    if bindings[VAR]) equivs.insert(FQAttribute= bindings[VAR]
    bindings[VAR]= FQAttribute
URIconstraint(URI) => Value:
    (Rel, Attr, Value) = match(URI, stemURI + '/' (\w+) '/' (\w+) '.' (\w+) '#record')
    some Alias = hash(Rel + URI)
    pk = primary_key_attribute(Rel)
    equivs.insert(Alias.pk= Value)
    joins.insert(Rel AS Attr)
    Value

  * FK = Map(FQAttribute(Employee, manager) => FQAttribute(http://hr.example/DB/Employee, id))
  * ?emp      <http://hr.example/DB/Employee#manager>    ?manager

(rel, attr) = match(P, stemURI + '/' + (\w+) + '#' (\w+))
  * rel => Employee  attr => manager
some alias = hash(Rel + S)
  * alias => "emp"
pk = primary_key_attribute(Rel)
  *-ignore, just use the paramater pk
    match S with
      VAR -> VARconstraint(S, Alias.pk)
  * eqS = (bindings[s => Alias.pk])
XXX   URI -> URIconstraint(S)
    match O with
      LITERAL -> equivs.insert(alias.attr= O)
      VAR -> VARconstraint(O, Alias.Attr)
      URI -> equivs.insert(Alias.Attr= URIconstraint(O))
      *
  * joins.insert(TableAlias(rel, alias)
joins.insert(rel AS alias ON eqS)
  * 
 * */
object RDB2RDF {
  case class R2RState(project:AttributeList, joins:List[Join], exprs:Expression, varmap:Map[Var, FQAttribute])

  def acc(state:R2RState, triple:TriplePattern):R2RState = {
    state
  }

  def apply (sparql:SparqlSelect, stem:StemURI, pk:PrimaryKey) : Select = {
    val SparqlSelect(attrs, triples) = sparql
    var r2rState = R2RState(
      // AttributeList(List()), 
      AttributeList(List(
	NamedAttribute(FQAttribute(Relation(Name("emp")),Attribute(Name("lastName"))),Attribute(Name("empName"))), 
	NamedAttribute(FQAttribute(Relation(Name("manager")),Attribute(Name("lastName"))),Attribute(Name("managName"))))), 
      // List[Join](), 
      List(
	Join(TableAlias(Relation(Name("Employee")),Relation(Name("manager"))),
	     Expression(List(
	       PrimaryExpressionEq(FQAttribute(Relation(Name("manager")),Attribute(Name("id"))),
				   RValueAttr(FQAttribute(Relation(Name("emp")),Attribute(Name("manager"))))))
		      ))
      ), 
      // Expression(List()), 
      Expression(List(
	PrimaryExpressionNotNull(FQAttribute(Relation(Name("emp")),Attribute(Name("lastName")))), 
	PrimaryExpressionNotNull(FQAttribute(Relation(Name("manager")),Attribute(Name("lastName"))))
      )), 
      Map[Var, FQAttribute]()
    )
    triples.triplepatterns.foreach(s => r2rState = acc(r2rState, s))
    val ret = Select(
      r2rState.project,
      TableList(
	TableAlias(Relation(Name("Employee")),Relation(Name("emp"))),
	r2rState.joins
      ),
      Some(r2rState.exprs)
    )
    ret
//     val ret = Select(
//       AttributeList(List(
// 	NamedAttribute(FQAttribute(Relation(Name("emp")),Attribute(Name("lastName"))),Attribute(Name("empName"))), 
// 	NamedAttribute(FQAttribute(Relation(Name("manager")),Attribute(Name("lastName"))),Attribute(Name("managName"))))),
//       TableList((Relation(Name("Employee")),Relation(Name("emp"))),
// //		r2rState.attrs
// 		TableList(
// 		  TableAlias(Relation(Name("Employee")),Relation(Name("emp"))),
// 		  List(
// 		    Join(TableAlias(Relation(Name("Employee")),Relation(Name("manager"))),
// 			 Expression(
// 			   List(PrimaryExpressionEq(FQAttribute(Relation(Name("manager")),Attribute(Name("id"))),
// 						    RValueAttr(FQAttribute(Relation(Name("emp")),Attribute(Name("manager"))))))))))
// 	      ),Some(Expression(List(PrimaryExpressionNotNull(FQAttribute(Relation(Name("emp")),Attribute(Name("lastName")))), PrimaryExpressionNotNull(FQAttribute(Relation(Name("manager")),Attribute(Name("lastName"))))))))
//     ret

  }
}
