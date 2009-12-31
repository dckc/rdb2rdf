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
  case class R2RState(joins:Set[AliasedResource], varmap:Map[Var, SQL2RDFValueMapper], exprs:Set[Expression])

  sealed abstract class SQL2RDFValueMapper(relaliasattr:RelAliasAttribute)
  case class IntMapper(relaliasattr:RelAliasAttribute) extends SQL2RDFValueMapper(relaliasattr)
  case class StringMapper(relaliasattr:RelAliasAttribute) extends SQL2RDFValueMapper(relaliasattr)
  case class DateMapper(relaliasattr:RelAliasAttribute) extends SQL2RDFValueMapper(relaliasattr)
  case class RDFNoder(relation:Relation, relaliasattr:RelAliasAttribute) extends SQL2RDFValueMapper(relaliasattr)
  case class RDFBNoder(relation:Relation, relaliasattr:RelAliasAttribute) extends SQL2RDFValueMapper(relaliasattr)

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

  def uriConstraint(state:R2RState, constrainMe:RelAliasAttribute, u:ObjUri, enforeForeignKeys:Boolean):R2RState = {
    // println("equiv+= " + toString(constrainMe) + "=" + value)
    //R2RState(state.joins, state.varmap, state.exprs + RelationalExpressionEq(constrainMe,RValueTyped(SQLDatatype.INTEGER,Name(u.v.s))))
    val relvar = if (enforeForeignKeys) RelAliasAttribute(constrainMe.relalias, Attribute(Name(u.attr.s))) else { println("constraining to " + constrainMe)
constrainMe }
    R2RState(state.joins, state.varmap, state.exprs + RelationalExpressionEq(relvar,RValueTyped(SQLDatatype.INTEGER,Name(u.v.s))))
  }

  def literalConstraint(state:R2RState, constrainMe:RelAliasAttribute, lit:SparqlLiteral, dt:SQLDatatype):R2RState = {
    // println("equiv+= " + toString(attr) + "=" + lit)
    R2RState(state.joins, state.varmap, state.exprs + RelationalExpressionEq(constrainMe,RValueTyped(dt,Name(lit.lit.lexicalForm))))    
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
      /* The variable has already been bound. */
      if (varToAttribute(state.varmap, v) == constrainMe)
	/* Don't bother stipulating that foo.bar=foo.bar . */
	state
      else
	/* Constraint against the initial binding for this variable. */
	R2RState(state.joins, state.varmap, state.exprs + RelationalExpressionEq(varToAttribute(state.varmap, v), RValueAttr(constrainMe)))
    } else {
      /* This is a new variable. */
      val binding = reldesc.primarykey match {
	case Some(Attribute(constrainMe.attribute.n)) => 
	  RDFNoder(rel, constrainMe)
	case _ => {
	  // e.g. Attribute(Name("id")) or None
	  if (reldesc.attributes.contains(constrainMe.attribute)) {
	    reldesc.attributes(constrainMe.attribute) match {
	      case ForeignKey(fkrel, fkattr) =>
		RDFNoder(rel, constrainMe)
	      case Value(SQLDatatype("Int")) =>
		IntMapper(constrainMe)
	      case Value(SQLDatatype("String")) =>
		StringMapper(constrainMe)
	      case Value(SQLDatatype("Date")) =>
		DateMapper(constrainMe)
	    }
	  } else {
	    RDFBNoder(rel, constrainMe)
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
      case IntMapper(relalias) => "INT: " + toString(relalias)
      case StringMapper(relalias) => "STRING: " + toString(relalias)
      case DateMapper(relalias) => "DATE: " + toString(relalias)
      case RDFNoder(relation, relalias) => "RDFNoder: " + relation.n.s + ", " + toString(relalias)
      case RDFBNoder(relation, relalias) => "RDFBNoder: " + relation.n.s + ", " + toString(relalias)
    }
  }

  def bindOnPredicate(db:DatabaseDesc, stateP:R2RState, triple:TriplePattern, pk:PrimaryKey, enforeForeignKeys:Boolean):R2RState = {
    val TriplePattern(s, p, o) = triple
    p match {
      case PVar(v) => error("variable predicates require tedious enumeration; too tedious for me.")
      case PUri(stem, spRel, spAttr) => {
	/* Attributes that come from the predicate: */
	val rel = Relation(Name(spRel.s))
	val attr = Attribute(Name(spAttr.s))
	val relalias = relAliasFromS(s)

	/* Attributes that come from the subject: */
	val subjattr = RelAliasAttribute(relalias, pk.attr)
	val objattr = RelAliasAttribute(relalias, attr)
	val state_postSubj = s match {
	  case SUri(u) => uriConstraint(stateP, subjattr, u, true)
	  case SVar(v) => varConstraint(stateP, subjattr, v, db, rel)
	}
	val state_subjJoin = R2RState(state_postSubj.joins + AliasedResource(rel,relalias), state_postSubj.varmap, state_postSubj.exprs)

	val (targetattr, targetrel, dt, state_fkeys) = db.relationdescs(rel).attributes(attr) match {
	  case ForeignKey(fkrel, fkattr) => {
	    val fkdt = db.relationdescs(fkrel).attributes(fkattr) match {
	      case ForeignKey(dfkrel, dfkattr) => error("foreign key " + rel.n + "." + attr.n + 
							"->" + fkrel.n + "." + fkattr.n + 
							"->" + dfkrel.n + "." + dfkattr.n)
	      case Value(x) => x
	    }
	    if (enforeForeignKeys) {
	      val oRelAlias = relAliasFromO(o)
	      val fkaliasattr = RelAliasAttribute(oRelAlias, fkattr)
	      val state_t = R2RState(state_subjJoin.joins + AliasedResource(fkrel,oRelAlias), state_subjJoin.varmap, state_subjJoin.exprs + RelationalExpressionEq(fkaliasattr,RValueAttr(objattr)))

	      (fkaliasattr, fkrel, fkdt, state_t)
	    } else {
	      (objattr, rel, fkdt, state_subjJoin)
	    }
	  }
	  case Value(dt) => (objattr, rel, dt, state_subjJoin)
	}
	o match {
	  case OLit(l) => literalConstraint(state_fkeys, targetattr, l, dt)
	  case OUri(u) => uriConstraint    (state_fkeys, targetattr, u, enforeForeignKeys)
	  case OVar(v) => varConstraint    (state_fkeys, targetattr, v, db, targetrel)
	}
      }
    }
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

  def findVars(gp:GraphPattern):Set[Var] = {
    gp match {
      case TableFilter(gp2:GraphPattern, expr:SparqlExpression) =>
	findVars(gp2)

      case TriplesBlock(triplepatterns) =>
	/* Examine each triple, updating the compilation state. */
	triplepatterns.foldLeft(Set[Var]())((x, y) => x ++ findVars(y))

      case x => error("no code to handle " + x)
    }
  }

  def varToAttribute(varmap:Map[Var, SQL2RDFValueMapper], vvar:Var):RelAliasAttribute = {
    varmap(vvar) match {
      case IntMapper(relalias) => relalias
      case StringMapper(relalias) => relalias
      case DateMapper(relalias) => relalias
      case RDFNoder(relation, relalias) => relalias
      case RDFBNoder(relation, relalias) => relalias
    }
  }

  def filter2expr(varmap:Map[Var, SQL2RDFValueMapper], f:SparqlPrimaryExpression):RelationalExpression = {
    val (lTerm:Term, rTerm:Term, sqlexpr) = f match { // sqlexpr::((RelAliasAttribute,RValueAttr)=>RelationalExpression)
      case SparqlPrimaryExpressionEq(l, r) => (l.term, r.term, RelationalExpressionEq(_,_))
      case SparqlPrimaryExpressionLt(l, r) => (l.term, r.term, RelationalExpressionLt(_,_))
    }
// RelationalExpressionEq(_,_) === (x,y) => PrymaryExpressionEq(x,y)
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

  def mapGraphPattern(db:DatabaseDesc, state:R2RState, gp:GraphPattern, pk:PrimaryKey, enforeForeignKeys:Boolean):R2RState = {
    gp match {
      case TableFilter(gp2:GraphPattern, expr:SparqlExpression) => {
	val state2 = mapGraphPattern(db, state, gp2, pk, enforeForeignKeys)

	/* Add constraints for all the FILTERS */
	val filterExprs:Set[RelationalExpression] =
	  expr.conjuncts.toSet map ((x:SparqlPrimaryExpression) => filter2expr(state2.varmap, x))

	R2RState(state2.joins, state2.varmap, state2.exprs ++ filterExprs)
      }
      case TriplesBlock(triplepatterns) => {
	/* Examine each triple, updating the compilation state. */
	val state2 = triplepatterns.foldLeft(state)((incState,s) => bindOnPredicate(db, incState, s, pk, enforeForeignKeys))
	val nullExprs = findVars(gp) map (vvar => RelationalExpressionNotNull(varToAttribute(state2.varmap, vvar)))
	R2RState(state2.joins, state2.varmap, state2.exprs ++ nullExprs)
      }
      case TableConjunction(list) => {
	list.foldLeft(state)((incState,s) => mapGraphPattern(db, incState, s, pk, enforeForeignKeys))
      }
      case TableDisjunction(list) => {
	val unionAlias = RelAlias(Name("R_union" + state.joins.size))
	val initDisjoints:Set[Select] = Set()
	val emptyState = R2RState(
	  Set[AliasedResource](), 
	  Map[Var, SQL2RDFValueMapper](), 
	  Set[Expression]()
	)
	val (state2, disjoints) = list.foldLeft((state, initDisjoints))((incPair,disjoint) => {
	  val (outerState, outerDisjoints) = incPair
	  val disjointState = mapGraphPattern(db, emptyState, disjoint, pk, enforeForeignKeys)
	  val disjointVars = findVars(disjoint)

	  val attrlist:Set[NamedAttribute] = disjointVars.foldLeft(Set[NamedAttribute]())((attrs, v) => 
	    attrs ++ Set(NamedAttribute(varToAttribute(disjointState.varmap, v), AttrAlias(Name("A_" + v.s)))))

	  val sel = Select(
	    AttributeList(attrlist),
	    TableList(disjointState.joins),
	    disjointState.exprs.size match {
	      case 0 => None
	      case 1 => Some(disjointState.exprs.toList(0))
	      case _ => Some(ExprConjunction(disjointState.exprs))
	    }
	  )
	  val outerState2 = disjointVars.foldLeft(outerState)((myState, v) => {
	    val unionAliasAttr = RelAliasAttribute(unionAlias, Attribute(Name("A_" + v.s)))
	    if (myState.varmap.contains(v)) {
	      /* The variable has already been bound. */
	      if (varToAttribute(myState.varmap, v) == unionAliasAttr)
		/* Same var was bound in an earlier disjoint. */
		myState
	      else
		/* Constraint against the initial binding for this variable. */
		R2RState(myState.joins, myState.varmap, myState.exprs + RelationalExpressionEq(varToAttribute(myState.varmap, v), RValueAttr(unionAliasAttr)))
	    } else {
	      /* This variable is new to the outer context. */
	      val mapper:SQL2RDFValueMapper = disjointState.varmap(v) match {
		case IntMapper(constrainMe)      => IntMapper(unionAliasAttr)
		case StringMapper(constrainMe)   => StringMapper(unionAliasAttr)
		case DateMapper(constrainMe)   => DateMapper(unionAliasAttr)
		case RDFNoder(rel, constrainMe)  => RDFNoder(rel, unionAliasAttr)
		case RDFBNoder(rel, constrainMe) => RDFBNoder(rel, unionAliasAttr)
	      }
	      R2RState(myState.joins, myState.varmap + (v -> mapper), myState.exprs)
	    }
	  })
	  (outerState2, outerDisjoints ++ Set(sel))
	})
	val union = Subselect(Union(disjoints))
	R2RState(state.joins + AliasedResource(union,unionAlias), state2.varmap, state2.exprs)
      }
      case x => error("no code to handle " + x)
    }
  }

  def apply (db:DatabaseDesc, sparql:SparqlSelect, stem:StemURI, pk:PrimaryKey, enforeForeignKeys:Boolean) : Select = {
    val SparqlSelect(attrs, triples) = sparql

    /* Create an object to hold our compilation state. */
    val initState = R2RState(
      Set[AliasedResource](), 
      Map[Var, SQL2RDFValueMapper](), 
      Set[Expression]()
    )

    val r2rState = mapGraphPattern(db, initState, sparql.gp, pk, enforeForeignKeys)

    /* Select the attributes corresponding to the variables
     * in the SPARQL SELECT.  */
    val attrlist:Set[NamedAttribute] = attrs.attributelist.foldLeft(Set[NamedAttribute]())((attrs, vvar) => 
      attrs ++ Set(NamedAttribute(varToAttribute(r2rState.varmap, vvar), AttrAlias(Name("A_" + vvar.s)))))

    /* Construct the generated query as an abstract syntax. */
    Select(
      AttributeList(attrlist),
      TableList(r2rState.joins),
      r2rState.exprs.size match {
	case 0 => None
	case 1 => Some(r2rState.exprs.toList(0))
	case _ => Some(ExprConjunction(r2rState.exprs))
      }
    )
  }
}
