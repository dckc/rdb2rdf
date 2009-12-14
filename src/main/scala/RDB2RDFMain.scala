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

sealed abstract class Binding
case class RDFNode(fqattr:RelAliasAttribute) extends Binding
case class Str(fqattr:RelAliasAttribute) extends Binding
case class Int(fqattr:RelAliasAttribute) extends Binding
case class Enum(fqattr:RelAliasAttribute) extends Binding

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
    some RelAlias = hash(Rel + URI)
    pk = primary_key_attribute(Rel)
    equivs.insert(RelAlias.pk= Value)
    joins.insert(Rel AS Attr)
    Value

  * FK = Map(FQAttribute(Employee, manager) => FQAttribute(http://hr.example/DB/Employee, id))
  * ?emp      <http://hr.example/DB/Employee#manager>    ?manager

(rel, attr) = match(P, stemURI + '/' + (\w+) + '#' (\w+))
  * rel => Employee  attr => manager
some relalias = hash(Rel + S)
  * relalias => "emp"
pk = primary_key_attribute(Rel)
  *-ignore, just use the paramater pk
    match S with
      VAR -> VARconstraint(S, RelAlias.pk)
  * eqS = (bindings[s => RelAlias.pk])
XXX   URI -> URIconstraint(S)
    match O with
      LITERAL -> equivs.insert(relalias.attr= O)
      VAR -> VARconstraint(O, RelAlias.Attr)
      URI -> equivs.insert(RelAlias.Attr= URIconstraint(O))
      *
  * joins.insert(RelAsRelAlias(rel, relalias)
joins.insert(rel AS relalias ON eqS)
  * 
 * */
object RDB2RDF {
  case class R2RState(project:AttributeList, joins:List[Join], exprs:Expression, varmap:Map[Var, RelAliasAttribute])

  def RelAliasFromS(s:S):RelAlias = {
    s match {
      case SUri(ob) => RelAliasFromNode(ob)
      case SVar(v) => RelAliasFromVar(v)
    }
  }

  def RelAliasFromO(o:O):Option[RelAlias] = {
    o match {
      case OUri(ob) => Some(RelAliasFromNode(ob))
      case OVar(v) => Some(RelAliasFromVar(v))
      case OLit(l) => None
    }
  }

  def RelAliasFromNode(u:ObjUri):RelAlias = {
    val ObjUri(stem, rel, Attr(a), CellValue(v)) = u
    RelAlias(Name(a + v))
  }

  def RelAliasFromVar(vr:Var):RelAlias = {
    val Var(v) = vr
    RelAlias(Name("R_" + v))
  }

  def URIconstraint(u:ObjUri, pk:PrimaryKey) = {
    val relalias = RelAliasFromNode(u)
    val ObjUri(stem, rel, attr, value) = u
    val fqattr = RelAliasAttribute(relalias, pk.attr)
    println("equiv+= " + toString(fqattr) + "=" + value)
  }

  /** varConstraint
   * called on triple pattern subjects and objects of type variable
   * passed the relation name (from predicate)
   *   if a subject, then the attribute is the primary key for relation
   *   if an object, then passed the attribute name (from predicate)
   * passed the relalias for this relation (e.g. _emp)
   *
   * schema(Employee.id) => (?emp => NodeTemplate("Employee", _emp.id) stemURI + rel + fk(rel) + value
   * schema(Employee.lastName) => (?lastName => RValueString(_emp.lastName)
   * schema(Employee.manater) => (?manager => ForeignKey("Employee", _manager.id)
   * 
   * SELECT ?emp WHERE { ?emp emp:manager <http://hr.example/our/favorite/DB/Employee/id.18#record> ; emp:name ?name }
   * SQL Results                     SPARQL Results
   * __emp __name    ?emp                                                      ?name
   * 4     "Bob"     <http://hr.example/our/favorite/DB/Employee/id.4#record>  "Bob"^^xsd:string
   * 6     "Sue"     <http://hr.example/our/favorite/DB/Employee/id.6#record>  "Sue"^^xsd:string
   * 
   * type String -> RDFStringConstructor // adds ^^xsd:string
   * type primary key -> RDFNodeConstructor // prefixes with stemURL + relation + attribute  and adds #record
   * */
  def varConstraint(v:Var, db:DatabaseDesc, rel:Relation, relalias:RelAlias, attr:Attribute) = {
    /* e.g.                                 Employee      _emp 	             id            
    **                                      Employee      _emp               lastName      
    **                                      Employee      _emp               manager       
    */
    val reldesc = db.relationdescs(rel)
    val mapper:String = reldesc.primarykey match {
      case Attribute(attr.n) => "RDFNoder(" + rel.n.s + ", "
      case _ => {
	reldesc.attributes(attr) match {
	  case ForeignKey(fkrel, fkattr) =>
	    "RDFNoder(" + rel.n.s + ", "
	  case Value(SQLDatatype(dt)) =>
	    dt + "Mapper("
	}
      }
    }
    println("?" + v.s + "=> " + mapper + relalias.n.s + "." + attr.n.s + ")")
    null
  }

  def LiteralConstraint(lit:SparqlLiteral, attr:RelAliasAttribute) = {
    println("equiv+= " + toString(attr) + "=" + lit)
  }

  def toString(fqattr:RelAliasAttribute) : String = {
    fqattr.relalias.n.s + "." + fqattr.attribute.n.s
  }
  // def toString(fqattr:RelAttribute) : String = {
  //   "[" + fqattr.relation.n.s + "]" + fqattr.attribute.n.s
  // }

  def acc(db:DatabaseDesc, state:R2RState, triple:TriplePattern, pk:PrimaryKey):R2RState = {
    val R2RState(project, joins, exprs, varmap) = state
    val TriplePattern(s, p, o) = triple
    p match {
      case PUri(stem, spRel, spAttr) => {
	val rel = Relation(Name(spRel.s))
	val attr = Attribute(Name(spAttr.s))
	val relalias = RelAliasFromS(s)
	println(rel.n.s + " AS " + relalias.n.s)
	s match {
	  case SUri(u) => URIconstraint(u, pk)
	  case SVar(v) => varConstraint(v, db, rel, relalias, pk.attr)
	  null
	}
	val objattr = RelAliasAttribute(relalias, attr)
	val target = db.relationdescs(rel).attributes(attr) match {
	  case ForeignKey(fkrel, fkattr) => {
	    o match {
	      case OUri(u) => {
		val oRelAlias = RelAliasFromNode(u)
		println(toString(objattr) + "->" + toString(RelAliasAttribute(oRelAlias, fkattr)))
		URIconstraint(u, pk)
	      }
	      case OVar(v) => {
		val oRelAlias = RelAliasFromVar(v)
		println(toString(objattr) + "->" + toString(RelAliasAttribute(oRelAlias, fkattr)))
		varConstraint(v, db, fkrel, oRelAlias, fkattr)
	      }
	      case OLit(l) => LiteralConstraint(l, objattr)
	    }
	  }
	  case Value(dt) => {
	    o match {
	      case OUri(u) => URIconstraint(u, pk)
	      case OVar(v) => varConstraint(v, db, rel, relalias, attr)
	      case OLit(l) => LiteralConstraint(l, objattr)
	    }
	  }
	}

      }
      case PVar(v) => error("variable predicates require tedious enumeration; too tedious for me.")
    }
    state
  }

  def apply (db:DatabaseDesc, sparql:SparqlSelect, stem:StemURI, pk:PrimaryKey) : Select = {
    val SparqlSelect(attrs, triples) = sparql
    var r2rState = R2RState(
      // AttributeList(List()), 
      AttributeList(List(
	NamedAttribute(RelAliasAttribute(RelAlias(Name("R_emp")),Attribute(Name("lastName"))),AttrAlias(Name("A_empName"))), 
	NamedAttribute(RelAliasAttribute(RelAlias(Name("R_manager")),Attribute(Name("lastName"))),AttrAlias(Name("A_managName"))))), 
      // List[Join](), 
      List(
	Join(RelAsRelAlias(Relation(Name("Employee")),RelAlias(Name("R_emp"))),None),
	Join(RelAsRelAlias(Relation(Name("Employee")),RelAlias(Name("R_manager"))),
	     Some(Expression(List(
	       PrimaryExpressionEq(RelAliasAttribute(RelAlias(Name("R_manager")),Attribute(Name("id"))),
				   RValueAttr(RelAliasAttribute(RelAlias(Name("R_emp")),Attribute(Name("manager"))))))
		      )))
      ), 
      // Expression(List()), 
      Expression(List(
	PrimaryExpressionNotNull(RelAliasAttribute(RelAlias(Name("R_emp")),Attribute(Name("lastName")))), 
	PrimaryExpressionNotNull(RelAliasAttribute(RelAlias(Name("R_manager")),Attribute(Name("lastName"))))
      )), 
      Map[Var, RelAliasAttribute]()
    )

    triples.triplepatterns.foreach(s => r2rState = acc(db, r2rState, s, pk))

    Select(
      r2rState.project,
      TableList(r2rState.joins),
      Some(r2rState.exprs)
    )
  }
}
