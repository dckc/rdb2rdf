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
  case class R2RState(joins:Set[AliasedResource], varmap:Map[Var, SQL2RDFValueMapper], exprs:Set[PrimaryExpression])

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

  def uriConstraint(state:R2RState, constrainMe:RelAliasAttribute, u:ObjUri):R2RState = {
    // println("equiv+= " + toString(constrainMe) + "=" + value)
    R2RState(state.joins, state.varmap, state.exprs + PrimaryExpressionEq(constrainMe,RValueTyped(SQLDatatype.INTEGER,Name(u.v.s))))    
  }

  def literalConstraint(state:R2RState, constrainMe:RelAliasAttribute, lit:SparqlLiteral, dt:SQLDatatype):R2RState = {
    // println("equiv+= " + toString(attr) + "=" + lit)
    R2RState(state.joins, state.varmap, state.exprs + PrimaryExpressionEq(constrainMe,RValueTyped(dt,Name(lit.lit.lexicalForm))))    
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
  def varConstraint(state:R2RState, constrainMe:RelAliasAttribute, v:Var, db:DatabaseDesc, rel:Relation):R2RState = {
    /* e.g.                                 Employee      _emp.id            
    **                                      Employee      _emp.lastName      
    **                                      Employee      _emp.manager       
    */
    val reldesc = db.relationdescs(rel)
    if (state.varmap.contains(v)) {
      if (varToAttribute(state.varmap, v) == constrainMe)
	state /* Don't bother stipulating that foo.bar=foo.bar . */
      else
	R2RState(state.joins, state.varmap, state.exprs + PrimaryExpressionEq(varToAttribute(state.varmap, v), RValueAttr(constrainMe)))
    } else {
      val binding = reldesc.primarykey match {
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
      R2RState(state.joins, state.varmap + (v -> binding), state.exprs)
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

  def acc(db:DatabaseDesc, stateP:R2RState, triple:TriplePattern, pk:PrimaryKey):R2RState = {
    val TriplePattern(s, p, o) = triple
    var state = stateP
    p match {
      case PVar(v) => error("variable predicates require tedious enumeration; too tedious for me.")
      case PUri(stem, spRel, spAttr) => {
	val rel = Relation(Name(spRel.s))
	val attr = Attribute(Name(spAttr.s))
	val relalias = relAliasFromS(s)
	val subjattr = RelAliasAttribute(relalias, pk.attr)
	val objattr = RelAliasAttribute(relalias, attr)

	s match {
	  case SUri(u) => state = uriConstraint(state, subjattr, u)
	  case SVar(v) => state = varConstraint(state, subjattr, v, db, rel)
	}
	state = R2RState(state.joins + AliasedResource(rel,relalias), state.varmap, state.exprs)

	db.relationdescs(rel).attributes(attr) match {
	  case ForeignKey(fkrel, fkattr) => {
	    val oRelAlias = relAliasFromO(o)
	    val fkaliasattr = RelAliasAttribute(oRelAlias, fkattr)
	    state = R2RState(state.joins, state.varmap, state.exprs + PrimaryExpressionEq(fkaliasattr,RValueAttr(objattr)))

	    var dt = db.relationdescs(fkrel).attributes(fkattr) match {
	      case ForeignKey(dfkrel, dfkattr) => error("foreign key " + rel.n + "." + attr.n + 
							"->" + fkrel.n + "." + fkattr.n + 
							"->" + dfkrel.n + "." + dfkattr.n)
	      case Value(x) => x
	    }
	    val conjuncts = o match {

	      /* Literal foreign keys should probably throw an error,
	       * instead does what user meant. */
	      case OLit(l) => state = literalConstraint(state, fkaliasattr, l, dt)
	      case OUri(u) => state = uriConstraint(state, fkaliasattr, u)
	      case OVar(v) => state = varConstraint(state, fkaliasattr, v, db, fkrel)
	    }

	    state = R2RState(state.joins + AliasedResource(fkrel,oRelAlias), state.varmap, state.exprs)
	  }
	  case Value(dt) => {
	    o match {
	      case OLit(l) => state = literalConstraint(state, objattr, l, dt)
	      case OUri(u) => state = uriConstraint(state, objattr, u)
	      case OVar(v) => state = varConstraint(state, objattr, v, db, rel)
	    }
	  }
	}
      }

    }
    state
  }

  def findVars(triple:TriplePattern):Set[Var] = {
    val TriplePattern(s, p, o) = triple
    val varS:Set[Var] = s match {
      case SVar(v) => Set(v)
      case _       => Set()
    }
    val varO:Set[Var] = o match {
      case OVar(v) => Set(v)
      case _       => Set()
    }
    varS ++ varO
  }

  def varToAttribute(varmap:Map[Var, SQL2RDFValueMapper], vvar:Var):RelAliasAttribute = {
    varmap(vvar) match {
      case StringMapper(relalias) => relalias
      case IntMapper(relalias) => relalias
      case RDFNoder(relation, relalias) => relalias
    }
  }

  def filter(varmap:Map[Var, SQL2RDFValueMapper], f:SparqlPrimaryExpression):PrimaryExpression = {
    val (lTerm:Term, rTerm:Term, sqlexpr:((RelAliasAttribute,RValueAttr)=>PrimaryExpression)) = f match {
      case SparqlPrimaryExpressionEq(l, r) => (l.term, r.term, PrimaryExpressionEq(_,_)) // Alex, how can i return PrimaryExpressionEq here?
      case SparqlPrimaryExpressionLt(l, r) => (l.term, r.term, PrimaryExpressionLt(_,_))
      }
// PrimaryExpressionEq(_,_) === (x,y) => PrymaryExpressionEq(x,y)
    lTerm match {
      // does not handle FILTER (<x> = ?v)
      case TermUri(obj) => error("only SPARQL PrimaryExpressions with a variable on the left have been implemented: punting on " + f)
      // FILTER (?v = <x> && ?v = ?x && ?v = 7)
      case TermVar(v) => { // :Var
	val l = varToAttribute(varmap, v)
	val r = rTerm match {
	  case TermUri(obj) => null // :ObjUri
	  case TermVar(v) => { // :Var
	    RValueAttr(varToAttribute(varmap, v))
	  }
	  case TermLit(lit) => null // :SparqlLiteral => RValueTyped(SQLDatatype, lit.n)
	}
	sqlexpr(l, r)
      }
      // does not handle FILTER (7 = ?v)
      case TermLit(lit) => error("only SPARQL PrimaryExpressions with a variable on the left have been implemented: punting on " + f)
    }
  }

  def nullGuard(varmap:Map[Var, SQL2RDFValueMapper], vvar:Var):PrimaryExpression = {
    val mapper:SQL2RDFValueMapper = varmap(vvar)
    val aattr = mapper match {
      case StringMapper(relalias) => relalias
      case IntMapper(relalias) => relalias
      case RDFNoder(relation, relalias) => relalias
    }
    PrimaryExpressionNotNull(aattr)
  }

  def apply (db:DatabaseDesc, sparql:SparqlSelect, stem:StemURI, pk:PrimaryKey) : Select = {
    val SparqlSelect(attrs, triples) = sparql

    /* Create an object to hold our compilation state. */
    var r2rState = R2RState(
      Set[AliasedResource](), 
      Map[Var, SQL2RDFValueMapper](), 
      Set[PrimaryExpression]()
    )

    /* Examine each triple, updating the compilation state. */
    triples.triplepatterns.foreach(s => r2rState = acc(db, r2rState, s, pk))

    /* Select the attributes corresponding to the variables
     * in the SPARQL SELECT.  */
    var attrlist:Set[NamedAttribute] = Set()
    attrs.attributelist.foreach(vvar => attrlist += 
      NamedAttribute(varToAttribute(r2rState.varmap, vvar), AttrAlias(Name("A_" + vvar.s)))
    )

    /* Add constraints for all the FILTERS */

    val filterExprs:Set[PrimaryExpression] =
      triples.filter.conjuncts.toSet map ((x:SparqlPrimaryExpression) => filter(r2rState.varmap, x))

    val allVars:Set[Var] = triples.triplepatterns.foldLeft(Set[Var]())((x, y) => x ++ findVars(y))
    val nullExprs = allVars map (nullGuard(r2rState.varmap, _))
    //val exprWithNull = allVars.foldLeft(exprs)((exprs,s) => nullGuard(exprs, r2rState.varmap, s))

    /* Construct the generated query as an abstract syntax. */
    Select(
      AttributeList(attrlist),
      TableList(r2rState.joins),
      Expression(r2rState.exprs ++ filterExprs ++ nullExprs)
//      Expression(exprWithNull)
    )
  }
}
