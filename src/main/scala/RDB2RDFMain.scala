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
case class Node(fqattr:FQAttribute) extends Binding
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
// object RDB2RDF {
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
    var alias = AliasFromNode(u)
    val ObjUri(stem, rel, attr, value) = u
    var fqattr = FQAttribute(alias, pk.attr)
    println("equiv|=" + fqattr + "=" + value)
  }

  def VarConstraint(v:Var, attr:FQAttribute) = {
    println("?" + v.s + "=> @@Binding(" + toString(attr) + ")")
  }

  def LiteralConstraint(lit:SparqlLiteral, attr:FQAttribute) = {
    println("equiv|=" + attr + "=" + lit)
  }

  def getKeyTarget(from:FQAttribute) : Option[FQAttribute] = {
    from match {
      case FQAttribute(Relation(Name("Employee")), Attribute(Name("manager"))) =>
	Some(FQAttribute(Relation(Name("Employee")), Attribute(Name("id"))))
      case FQAttribute(Relation(Name("Employee")), Attribute(Name("lastName"))) => None
    }
  }

  def toString(fqattr:FQAttribute) : String = {
    fqattr.relation.n.s + "." + fqattr.attribute.n.s
  }

  def acc(state:R2RState, triple:TriplePattern, pk:PrimaryKey):R2RState = {
    val R2RState(project, joins, exprs, varmap) = state
    val TriplePattern(s, p, o) = triple
    p match {
      case PUri(stem, spRel, spAttr) => {
	var rel = Relation(Name(spRel.s))
	var attr = Attribute(Name(spAttr.s))
	var alias = AliasFromS(s)
	println(rel.n.s + " AS " + alias.n.s)
	s match {
	  case SUri(u) => URIconstraint(u, pk)
	  case SVar(v) => VarConstraint(v, FQAttribute(alias, pk.attr))
	  null
	}
	var objattr = FQAttribute(alias, attr)
	var oAlias = AliasFromO(o) // None if OLit
	var target = getKeyTarget(FQAttribute(rel, attr))
	target match {
	  case None => null
	  case Some(fqattr) => {
	    oAlias match {
	      case None => error("no oAlias for foreign key " + toString(fqattr))
	      case Some(a) => {
		println(toString(objattr) + "->" + toString(FQAttribute(a, fqattr.attribute)))
		1 // null ptr error otherwise. what's getting assigned to this?
	      }
	    }
	  }
	}
	o match {
	  case OUri(u) => URIconstraint(u, pk)
	  case OVar(v) => VarConstraint(v, objattr)
	  case OLit(l) => LiteralConstraint(l, objattr)
	  null
	}
	null
      }
      case PVar(v) => error("variable predicates require tedious enumeration; too tedious for me.")
    }
    state
  }

//   def apply (sparql:SparqlSelect, stem:StemURI, pk:PrimaryKey) : Select = {
//     val SparqlSelect(attrs, triples) = sparql
  def apply (sparql:SparqlSelect, stem:StemURI, pk:PrimaryKey) : Select = {
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

    triples.triplepatterns.foreach(s => r2rState = acc(r2rState, s, pk))

    Select(
      r2rState.project,
      TableList(r2rState.joins),
      Some(r2rState.exprs)
    )
  }
}
