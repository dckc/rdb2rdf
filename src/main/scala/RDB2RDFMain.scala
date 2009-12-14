package w3c.sw

import scala.util.parsing.combinator._
import java.net.URI

case class StemURI(s:String)
case class PrimaryKey(attr:Attribute)

sealed abstract class Binding
case class RDFNode(fqattr:RelAliasAttribute) extends Binding
case class Str(fqattr:RelAliasAttribute) extends Binding
case class Int(fqattr:RelAliasAttribute) extends Binding
case class Enum(fqattr:RelAliasAttribute) extends Binding

object RDB2RDF {
  case class R2RState(project:AttributeList, joins:List[Join], exprs:Expression, varmap:Map[Var, RelAliasAttribute])

  def relAliasFromS(s:S):RelAlias = {
    s match {
      case SUri(ob) => relAliasFromNode(ob)
      case SVar(v) => relAliasFromVar(v)
    }
  }

  def relAliasFromNode(u:ObjUri):RelAlias = {
    val ObjUri(stem, rel, Attr(a), CellValue(v)) = u
    RelAlias(Name(a + v))
  }

  def relAliasFromVar(vr:Var):RelAlias = {
    val Var(v) = vr
    RelAlias(Name("R_" + v))
  }

  def uriConstraint(u:ObjUri, pk:PrimaryKey) = {
    val relalias = relAliasFromNode(u)
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
   * schema(Employee.id) => (?emp => NodeTemplate("Employee", R_emp.id) stemURI + rel + fk(rel) + value
   * schema(Employee.lastName) => (?lastName => RValueString(_Remp.lastName)
   * schema(Employee.manater) => (?manager => ForeignKey("Employee", R_manager.id)
   * 
   * SELECT ?emp WHERE { ?emp emp:manager <http://hr.example/our/favorite/DB/Employee/id.18#record> ; emp:name ?name }
   * SQL Results                     SPARQL Results
   * A_emp A_name    ?emp                                                      ?name
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

  def literalConstraint(lit:SparqlLiteral, attr:RelAliasAttribute) = {
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
	val relalias = relAliasFromS(s)
	println(rel.n.s + " AS " + relalias.n.s)
	s match {
	  case SUri(u) => uriConstraint(u, pk)
	  case SVar(v) => varConstraint(v, db, rel, relalias, pk.attr)
	  null
	}
	val objattr = RelAliasAttribute(relalias, attr)
	val target = db.relationdescs(rel).attributes(attr) match {
	  case ForeignKey(fkrel, fkattr) => {
	    o match {
	      case OUri(u) => {
		val oRelAlias = relAliasFromNode(u)
		println(toString(objattr) + "->" + toString(RelAliasAttribute(oRelAlias, fkattr)))
		uriConstraint(u, pk)
	      }
	      case OVar(v) => {
		val oRelAlias = relAliasFromVar(v)
		println(toString(objattr) + "->" + toString(RelAliasAttribute(oRelAlias, fkattr)))
		varConstraint(v, db, fkrel, oRelAlias, fkattr)
	      }
	      case OLit(l) => literalConstraint(l, objattr)
	    }
	  }
	  case Value(dt) => {
	    o match {
	      case OUri(u) => uriConstraint(u, pk)
	      case OVar(v) => varConstraint(v, db, rel, relalias, attr)
	      case OLit(l) => literalConstraint(l, objattr)
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
