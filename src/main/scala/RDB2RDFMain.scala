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
  case class R2RState(joined:Set[RelAlias], allVars:List[Var], inConstraint:Set[Var], joins:List[Join], varmap:Map[Var, SQL2RDFValueMapper], exprs:List[PrimaryExpression])

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

  def relAliasFromO(o:O):RelAlias = {
    o match {
      case OUri(ob) => relAliasFromNode(ob)
      case OVar(v) => relAliasFromVar(v)
      case OLit(l) => relAliasFromLiteral(l)
    }
  }

  def relAliasFromNode(u:ObjUri):RelAlias = {
    val ObjUri(stem, rel, Attr(a), CellValue(v)) = u
    RelAlias(Name("R_" + a + v))
  }

  def relAliasFromLiteral(l:SparqlLiteral):RelAlias = {
    RelAlias(Name("R_" + l.lit.lexicalForm))
  }

  def relAliasFromVar(vr:Var):RelAlias = {
    val Var(v) = vr
    RelAlias(Name("R_" + v))
  }

  def uriConstraint(constrainMe:RelAliasAttribute, u:ObjUri):Expression = {
    // println("equiv+= " + toString(constrainMe) + "=" + value)
    Expression(List(PrimaryExpressionEq(constrainMe,RValueTyped(SQLDatatype.INTEGER,Name(u.v.s)))))
  }

  def literalConstraint(constrainMe:RelAliasAttribute, lit:SparqlLiteral, dt:SQLDatatype):Expression = {
    // println("equiv+= " + toString(attr) + "=" + lit)
    Expression(List(PrimaryExpressionEq(constrainMe,RValueTyped(dt,Name(lit.lit.lexicalForm)))))
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
  def varConstraint(constrainMe:RelAliasAttribute, v:Var, db:DatabaseDesc, rel:Relation):SQL2RDFValueMapper = {
    /* e.g.                                 Employee      _emp.id            
    **                                      Employee      _emp.lastName      
    **                                      Employee      _emp.manager       
    */
    val reldesc = db.relationdescs(rel)
    reldesc.primarykey match {
      case Attribute(constrainMe.attribute.n) => 
	RDFNoder(rel, constrainMe)
      case _ => {
	reldesc.attributes(constrainMe.attribute) match {
	  case ForeignKey(fkrel, fkattr) =>
	    RDFNoder(rel, constrainMe)
	  case Value(SQLDatatype("String")) =>
	    StringMapper(constrainMe)
	  case Value(SQLDatatype("Int")) =>
	    IntMapper(constrainMe)
	}
      }
    }
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
    var R2RState(joined, allVars, inConstraint, joins, varmap, exprs) = state
    val TriplePattern(s, p, o) = triple
    p match {
      case PUri(stem, spRel, spAttr) => {
	val rel = Relation(Name(spRel.s))
	val attr = Attribute(Name(spAttr.s))
	val relalias = relAliasFromS(s)
	val subjattr = RelAliasAttribute(relalias, pk.attr)
	val objattr = RelAliasAttribute(relalias, attr)

	// println(rel.n.s + " AS " + relalias.n.s)
	val sconstraint:Option[Expression] = s match {
	  case SUri(u) => {
	    uriConstraint(subjattr, u)
	    // joins = joins ::: List(Join(RelAsRelAlias(Relation(Name("Employee")),RelAlias(Name("R_emp"))),None))
	    None
	  }
	  case SVar(v) => {
	    val binding:SQL2RDFValueMapper = varConstraint(subjattr, v, db, rel)
	    varmap += v -> binding
	    // println(toString(binding))
	    None
	  }
	}
	joined contains(relalias) match {
	  case false => {
	    joins = joins ::: List(Join(RelAsRelAlias(rel,relalias), sconstraint))
	    joined = joined + relalias
	  }
	  case true => null
	}
	val target = db.relationdescs(rel).attributes(attr) match {
	  case ForeignKey(fkrel, fkattr) => {
	    val oRelAlias = relAliasFromO(o)
	    val fkaliasattr = RelAliasAttribute(oRelAlias, fkattr)
	    val joinconstraint = PrimaryExpressionEq(fkaliasattr,RValueAttr(objattr))

	    var dt = db.relationdescs(fkrel).attributes(fkattr) match {
	      case ForeignKey(dfkrel, dfkattr) => error("foreign key " + rel.n + "." + attr.n + 
							"->" + fkrel.n + "." + fkattr.n + 
							"->" + dfkrel.n + "." + dfkattr.n)
	      case Value(x) => x
	    }
	    val conjuncts = o match {

	      /* Literal foreign keys should probably throw an error,
	       * instead does what user meant. */
	      case OLit(l) => List(joinconstraint) ::: literalConstraint(fkaliasattr, l, dt).conjuncts

	      case OUri(u) => List(joinconstraint) ::: uriConstraint(fkaliasattr, u).conjuncts

	      case OVar(v) => {
		val binding = varConstraint(fkaliasattr, v, db, fkrel)
		varmap += v -> binding
		List(joinconstraint)
	      }
	    }

	    joined contains(oRelAlias) match {
	      case false => {
		joins = joins ::: List(Join(RelAsRelAlias(fkrel,oRelAlias), Some(Expression(conjuncts))))
		joined = joined + oRelAlias
	      }
	      case true => null
	    }
	  }
	  case Value(dt) => {
	    o match {
	      case OLit(l) => {
		val c = literalConstraint(objattr, l, dt).conjuncts
		exprs = exprs ::: c
	      }
	      case OUri(u) => uriConstraint(objattr, u)
	      case OVar(v) => {
		allVars = allVars ::: List(v)
		// !! 2nd+ ref implies constraint
		val binding = varConstraint(objattr, v, db, rel)
		varmap += v -> binding
	      }
	    }
	  }
	}

      }
      case PVar(v) => error("variable predicates require tedious enumeration; too tedious for me.")
    }
    R2RState(joined, allVars, inConstraint, joins, varmap, exprs)
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

  def nullGuard(notNulls:List[PrimaryExpression], inConstraint:Set[Var], varmap:Map[Var, SQL2RDFValueMapper], vvar:Var):List[PrimaryExpression] = {
    var ret = notNulls
    inConstraint contains(vvar) match {
      case false => {
	val mapper:SQL2RDFValueMapper = varmap(vvar)
	val aattr = mapper match {
	  case StringMapper(relalias) => relalias
	  case IntMapper(relalias) => relalias
	  case RDFNoder(relation, relalias) => relalias
	}
	ret = ret ::: List(PrimaryExpressionNotNull(aattr))
      }
      case true => null
    }
    ret
  }

  def apply (db:DatabaseDesc, sparql:SparqlSelect, stem:StemURI, pk:PrimaryKey) : Select = {
    val SparqlSelect(attrs, triples) = sparql

    /* Create an object to hold our compilation state. */
    var r2rState = R2RState(
      Set[RelAlias](), 
      List[Var](), 
      Set[Var](), 
      List[Join](), 
      Map[Var, SQL2RDFValueMapper](), 
      List[PrimaryExpression]()
    )

    /* Examine each triple, updating the compilation state. */
    triples.triplepatterns.foreach(s => r2rState = acc(db, r2rState, s, pk))

    /* Select the attributes corresponding to the variables
     * in the SPARQL SELECT.  */
    var attrlist:List[NamedAttribute] = List()
    attrs.attributelist.foreach(s => attrlist = attrlist ::: List(project(r2rState.varmap, s)))

    /* Add null guards for attributes associated with variables which
     * are not optional and have not been used in constraints. */
    var notNulls:List[PrimaryExpression] = r2rState.exprs
    r2rState.allVars.foreach(s => notNulls = nullGuard(notNulls, r2rState.inConstraint, r2rState.varmap, s))
    val where = notNulls.size match {
      case 0 => None
      case _ => Some(Expression(notNulls))
    }

    /* Construct the generated query as an abstract syntax. */
    Select(
      AttributeList(attrlist),
      TableList(r2rState.joins),
      where
    )
  }
}
