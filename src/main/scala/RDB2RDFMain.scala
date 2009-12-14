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
case class RDFNode(fqattr:FQAttribute) extends Binding
case class Str(fqattr:FQAttribute) extends Binding
case class Int(fqattr:FQAttribute) extends Binding
case class Enum(fqattr:FQAttribute) extends Binding

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

  def AliasFromS(s:S):Relation = {
    s match {
      case SUri(ob) => AliasFromNode(ob)
      case SVar(v) => AliasFromVar(v)
    }
  }

  def AliasFromO(o:O):Option[Relation] = {
    o match {
      case OUri(ob) => Some(AliasFromNode(ob))
      case OVar(v) => Some(AliasFromVar(v))
      case OLit(l) => None
    }
  }

  def AliasFromNode(u:ObjUri):Relation = {
    val ObjUri(stem, rel, Attr(a), CellValue(v)) = u
    Relation(Name(a + v))
  }

  def AliasFromVar(vr:Var):Relation = {
    val Var(v) = vr
    Relation(Name("_" + v))
  }

  def URIconstraint(u:ObjUri, pk:PrimaryKey) = {
    val alias = AliasFromNode(u)
    val ObjUri(stem, rel, attr, value) = u
    val fqattr = FQAttribute(alias, pk.attr)
    println("equiv|=" + fqattr + "=" + value)
  }

  /** varConstraint
   * called on triple pattern subjects and objects of type variable
   * passed the relation name (from predicate)
   *   if a subject, then the attribute is the primary key for relation
   *   if an object, then passed the attribute name (from predicate)
   * passed the alias for this relation (e.g. _emp)
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
  def varConstraint(v:Var, db:DatabaseDesc, rel:Relation, alias:Relation, attr:Attribute) = {
    /* e.g.                                 Employee      _emp 	          id            
    **                                      Employee      _emp            lastName      
    **                                      Employee      _emp            manager       
    */
    val reldesc = db.relationdescs(rel)
    val mapper:String = reldesc.attributes(attr) match {
      case ForeignKey(fkrel, fkattr) =>
	"RDFNode(" + rel.n.s + ", "
      case Value(SQLDatatype(dt)) =>
	dt + "Mapper("
    }
    println("?" + v.s + "=> " + mapper + alias.n.s + "." + attr.n.s + ")")
    null
  }

  def LiteralConstraint(lit:SparqlLiteral, attr:FQAttribute) = {
    println("equiv|=" + attr + "=" + lit)
  }

  def toString(fqattr:FQAttribute) : String = {
    fqattr.relation.n.s + "." + fqattr.attribute.n.s
  }

  def acc(db:DatabaseDesc, state:R2RState, triple:TriplePattern, pk:PrimaryKey):R2RState = {
    val R2RState(project, joins, exprs, varmap) = state
    val TriplePattern(s, p, o) = triple
    p match {
      case PUri(stem, spRel, spAttr) => {
	val rel = Relation(Name(spRel.s))
	val attr = Attribute(Name(spAttr.s))
	val alias = AliasFromS(s)
	println(rel.n.s + " AS " + alias.n.s)
	s match {
	  case SUri(u) => URIconstraint(u, pk)
	  case SVar(v) => varConstraint(v, db, rel, alias, pk.attr)
	  null
	}
	val objattr = FQAttribute(alias, attr)
	val target = db.relationdescs(rel).attributes(attr) match {
	  case ForeignKey(fkrel, fkattr) => {
	    val fqattr = FQAttribute(fkrel, fkattr)
	    o match {
	      case OUri(u) => {
		val oAlias = AliasFromNode(u)
		println(toString(objattr) + "->" + toString(FQAttribute(oAlias, fqattr.attribute)))
		URIconstraint(u, pk)
	      }
	      case OVar(v) => {
		val oAlias = AliasFromVar(v)
		println(toString(objattr) + "->" + toString(FQAttribute(oAlias, fqattr.attribute)))
		varConstraint(v, db, fqattr.relation, oAlias, fqattr.attribute)
	      }
	      case OLit(l) => LiteralConstraint(l, objattr)
	    }
	  }
	  case Value(dt) => {
	    o match {
	      case OUri(u) => URIconstraint(u, pk)
	      case OVar(v) => varConstraint(v, db, rel, alias, attr)
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
	NamedAttribute(FQAttribute(Relation(Name("emp")),Attribute(Name("lastName"))),Attribute(Name("empName"))), 
	NamedAttribute(FQAttribute(Relation(Name("manager")),Attribute(Name("lastName"))),Attribute(Name("managName"))))), 
      // List[Join](), 
      List(
	Join(TableAlias(Relation(Name("Employee")),Relation(Name("emp"))),None),
	Join(TableAlias(Relation(Name("Employee")),Relation(Name("manager"))),
	     Some(Expression(List(
	       PrimaryExpressionEq(FQAttribute(Relation(Name("manager")),Attribute(Name("id"))),
				   RValueAttr(FQAttribute(Relation(Name("emp")),Attribute(Name("manager"))))))
		      )))
      ), 
      // Expression(List()), 
      Expression(List(
	PrimaryExpressionNotNull(FQAttribute(Relation(Name("emp")),Attribute(Name("lastName")))), 
	PrimaryExpressionNotNull(FQAttribute(Relation(Name("manager")),Attribute(Name("lastName"))))
      )), 
      Map[Var, FQAttribute]()
    )

    triples.triplepatterns.foreach(s => r2rState = acc(db, r2rState, s, pk))

    Select(
      r2rState.project,
      TableList(r2rState.joins),
      Some(r2rState.exprs)
    )
  }
}
