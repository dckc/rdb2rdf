package w3c.sw

import scala.util.parsing.combinator._
import java.net.URI

case class StemURI(s:String)
case class PrimaryKey(attr:Attribute)

sealed abstract class Binding
case class RDFNode(relaliasattr:RelAliasAttribute) extends Binding
case class Str(relaliasattr:RelAliasAttribute) extends Binding
case class Int(relaliasattr:RelAliasAttribute) extends Binding
case class Enum(relaliasattr:RelAliasAttribute) extends Binding

object RDB2RDF {
  case class R2RState(joined:Set[RelAlias], joins:List[Join], exprs:Expression, varmap:Map[Var, SQL2RDFValueMapper])

  sealed abstract class SQL2RDFValueMapper(relaliasattr:RelAliasAttribute)
  case class StringMapper(relaliasattr:RelAliasAttribute) extends SQL2RDFValueMapper(relaliasattr)
  case class IntMapper(relaliasattr:RelAliasAttribute) extends SQL2RDFValueMapper(relaliasattr)
  case class RDFNoder(relation:Relation, relaliasattr:RelAliasAttribute) extends SQL2RDFValueMapper(relaliasattr)

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

  def uriConstraint(u:ObjUri, pk:PrimaryKey):Expression = {
    val relalias = relAliasFromNode(u)
    val ObjUri(stem, rel, attr, value) = u
    val relaliasattr = RelAliasAttribute(relalias, pk.attr)
    println("equiv+= " + toString(relaliasattr) + "=" + value)
    Expression(List(PrimaryExpressionEq(relaliasattr,RValueTyped(SQLDatatype.INTEGER,Name(value.s)))))
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
  def varConstraint(v:Var, db:DatabaseDesc, rel:Relation, relalias:RelAlias, attr:Attribute):SQL2RDFValueMapper = {
    /* e.g.                                 Employee      _emp 	             id            
    **                                      Employee      _emp               lastName      
    **                                      Employee      _emp               manager       
    */
    val reldesc = db.relationdescs(rel)
    val aattr = RelAliasAttribute(relalias, attr)
    reldesc.primarykey match {
      case Attribute(attr.n) => 
	RDFNoder(rel, aattr)
      case _ => {
	reldesc.attributes(attr) match {
	  case ForeignKey(fkrel, fkattr) =>
	    RDFNoder(rel, aattr)
	  case Value(SQLDatatype("String")) =>
	    StringMapper(aattr)
	  case Value(SQLDatatype("Int")) =>
	    IntMapper(aattr)
	}
      }
    }
  }

  def literalConstraint(lit:SparqlLiteral, attr:RelAliasAttribute) = {
    println("equiv+= " + toString(attr) + "=" + lit)
  }

  def toString(relaliasattr:RelAliasAttribute) : String = {
    relaliasattr.relalias.n.s + "." + relaliasattr.attribute.n.s
  }
  def toString(mapper:SQL2RDFValueMapper) : String = {
    mapper match {
      case StringMapper(relalias) => "STRING: " + toString(relalias)
      case IntMapper(relalias) => "INT: " + toString(relalias)
      case RDFNoder(relation, relalias) => "RDFNoder: " + relation.n.s + ", " + toString(relalias)
    }
  }
  // def toString(relaliasattr:RelAttribute) : String = {
  //   "[" + relaliasattr.relation.n.s + "]" + relaliasattr.attribute.n.s
  // }

  def acc(db:DatabaseDesc, state:R2RState, triple:TriplePattern, pk:PrimaryKey):R2RState = {
    var R2RState(joined, joins, exprs, varmap) = state
    val TriplePattern(s, p, o) = triple
    p match {
      case PUri(stem, spRel, spAttr) => {
	val rel = Relation(Name(spRel.s))
	val attr = Attribute(Name(spAttr.s))
	val relalias = relAliasFromS(s)
	println(rel.n.s + " AS " + relalias.n.s)
	val sconstraint:Option[Expression] = s match {
	  case SUri(u) => {
	    uriConstraint(u, pk)
	    // joins = joins ::: List(Join(RelAsRelAlias(Relation(Name("Employee")),RelAlias(Name("R_emp"))),None))
	    None
	  }
	  case SVar(v) => {
	    val binding:SQL2RDFValueMapper = varConstraint(v, db, rel, relalias, pk.attr)
	    varmap += v -> binding
	    println(toString(binding))
	  }
	  None
	}
	joined contains(relalias) match {
	  case false => {
	    joins = joins ::: List(Join(RelAsRelAlias(rel,relalias), sconstraint))
	    joined = joined + relalias
	  }
	  case true => null
	}
	val objattr = RelAliasAttribute(relalias, attr)
	val target = db.relationdescs(rel).attributes(attr) match {
	  case ForeignKey(fkrel, fkattr) => {
	    o match {
	      case OUri(u) => {
		val oRelAlias = relAliasFromNode(u)
		println(toString(objattr) + "->" + toString(RelAliasAttribute(oRelAlias, fkattr)))
		println(fkrel.n.s + " AS " + oRelAlias.n.s)
		val ret = Some(uriConstraint(u, pk))
		joined contains(oRelAlias) match {
		  case false => {
		    joins = joins ::: List(Join(RelAsRelAlias(fkrel,oRelAlias), ret))
		    joined = joined + oRelAlias
		  }
		  case true => null
		}
	      }
	      case OVar(v) => {
		val oRelAlias = relAliasFromVar(v)
		val fkaliasattr = RelAliasAttribute(oRelAlias, fkattr)
		println(toString(objattr) + "->" + toString(fkaliasattr))
		val binding = varConstraint(v, db, fkrel, oRelAlias, fkattr)
		varmap += v -> binding
		println(toString(binding))
		println(fkrel.n.s + " AS " + oRelAlias.n.s)
		val ret = Some(Expression(List(PrimaryExpressionEq(fkaliasattr,RValueAttr(objattr)))))
		joined contains(oRelAlias) match {
		  case false => {
		    joins = joins ::: List(Join(RelAsRelAlias(fkrel,oRelAlias), ret))
		    joined = joined + oRelAlias
		  }
		  case true => null
		}
	      }
	      case OLit(l) => {
		literalConstraint(l, objattr)
	      }
	    }
	  }
	  case Value(dt) => {
	    o match {
	      case OUri(u) => uriConstraint(u, pk)
	      case OVar(v) => {
		val binding = varConstraint(v, db, rel, relalias, attr)
		varmap += v -> binding
		println(toString(binding))
	      }
	      case OLit(l) => literalConstraint(l, objattr)
	    }
	  }
	}

      }
      case PVar(v) => error("variable predicates require tedious enumeration; too tedious for me.")
    }
    R2RState(joined, joins, exprs, varmap)
  }

  def project(varmap:Map[Var, SQL2RDFValueMapper], vvar:Var):NamedAttribute = {
    val mapper:SQL2RDFValueMapper = varmap(vvar)
    val aattr = mapper match {
      case StringMapper(relalias) => relalias
      case IntMapper(relalias) => relalias
      case RDFNoder(relation, relalias) => relalias
    }
    NamedAttribute(aattr, AttrAlias(Name("A_" + vvar.s)))
  }

  def apply (db:DatabaseDesc, sparql:SparqlSelect, stem:StemURI, pk:PrimaryKey) : Select = {
    val SparqlSelect(attrs, triples) = sparql
    var r2rState = R2RState(
      Set[RelAlias](), 
      List[Join](), 
      // Expression(List()), 
      Expression(List(
	PrimaryExpressionNotNull(RelAliasAttribute(RelAlias(Name("R_emp")),Attribute(Name("lastName")))), 
	PrimaryExpressionNotNull(RelAliasAttribute(RelAlias(Name("R_manager")),Attribute(Name("lastName"))))
      )), 
      Map[Var, SQL2RDFValueMapper]()
    )

    triples.triplepatterns.foreach(s => r2rState = acc(db, r2rState, s, pk))
    println("joins: " + r2rState.joins)

    var attrlist:List[NamedAttribute] = List()
    attrs.attributelist.foreach(s => attrlist = attrlist ::: List(project(r2rState.varmap, s)))

    Select(
      AttributeList(attrlist),
      TableList(r2rState.joins),
      Some(r2rState.exprs)
    )
  }
}
