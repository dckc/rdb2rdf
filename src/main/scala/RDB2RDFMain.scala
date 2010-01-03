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
  case class R2RState(joins:AddOrderedSet[Join], varmap:Map[Var, SQL2RDFValueMapper], exprs:Set[Expression])

  sealed abstract class SQL2RDFValueMapper(relaliasattr:RelAliasAttribute, disjoints:Set[RelationalExpressionNe])
  case class IntMapper(relaliasattr:RelAliasAttribute, disjoints:Set[RelationalExpressionNe]) extends SQL2RDFValueMapper(relaliasattr, disjoints)
  case class StringMapper(relaliasattr:RelAliasAttribute, disjoints:Set[RelationalExpressionNe]) extends SQL2RDFValueMapper(relaliasattr, disjoints)
  case class DateMapper(relaliasattr:RelAliasAttribute, disjoints:Set[RelationalExpressionNe]) extends SQL2RDFValueMapper(relaliasattr, disjoints)
  case class RDFNoder(relation:Relation, relaliasattr:RelAliasAttribute, disjoints:Set[RelationalExpressionNe]) extends SQL2RDFValueMapper(relaliasattr, disjoints)
  case class RDFBNoder(relation:Relation, relaliasattr:RelAliasAttribute, disjoints:Set[RelationalExpressionNe]) extends SQL2RDFValueMapper(relaliasattr, disjoints)

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
    //R2RState(state.joins, state.varmap, state.exprs + RelationalExpressionEq(constrainMe,PrimaryExpressionTyped(SQLDatatype.INTEGER,Name(u.v.s))))
    val relvar = if (enforeForeignKeys) RelAliasAttribute(constrainMe.relalias, Attribute(Name(u.attr.s))) else constrainMe
    R2RState(state.joins, state.varmap, state.exprs + RelationalExpressionEq(PrimaryExpressionAttr(relvar),PrimaryExpressionTyped(SQLDatatype.INTEGER,Name(u.v.s))))
  }

  def literalConstraint(state:R2RState, constrainMe:RelAliasAttribute, lit:SparqlLiteral, dt:SQLDatatype):R2RState = {
    // println("equiv+= " + toString(attr) + "=" + lit)
    R2RState(state.joins, state.varmap, state.exprs + RelationalExpressionEq(PrimaryExpressionAttr(constrainMe),PrimaryExpressionTyped(dt,Name(lit.lit.lexicalForm))))    
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
	state // !!! what about disjoints?
      else {
	/* Constraint against the initial binding for this variable. */
	val constraint = RelationalExpressionEq(PrimaryExpressionAttr(varToAttribute(state.varmap, v)), PrimaryExpressionAttr(constrainMe))
	R2RState(state.joins, state.varmap, 
		 if (varToAttributeDisjoints(state.varmap, v).size > 0) {
		   state.exprs ++ {varToAttributeDisjoints(state.varmap, v) map ((d) => ExprDisjunction(Set(d, constraint)))}
		 } else
		   state.exprs + constraint
	       )
      }
    } else {
      /* This is a new variable. */
      val binding = reldesc.primarykey match {
	case Some(Attribute(constrainMe.attribute.n)) => 
	  RDFNoder(rel, constrainMe, Set())
	case _ => {
	  // e.g. Attribute(Name("id")) or None
	  if (reldesc.attributes.contains(constrainMe.attribute)) {
	    reldesc.attributes(constrainMe.attribute) match {
	      case ForeignKey(fkrel, fkattr) =>
		RDFNoder(rel, constrainMe, Set())
	      case Value(SQLDatatype("Int")) =>
		IntMapper(constrainMe, Set())
	      case Value(SQLDatatype("String")) =>
		StringMapper(constrainMe, Set())
	      case Value(SQLDatatype("Date")) =>
		DateMapper(constrainMe, Set())
	    }
	  } else {
	    RDFBNoder(rel, constrainMe, Set())
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
      case IntMapper(relalias, disjoints) => "INT: " + toString(relalias)
      case StringMapper(relalias, disjoints) => "STRING: " + toString(relalias)
      case DateMapper(relalias, disjoints) => "DATE: " + toString(relalias)
      case RDFNoder(relation, relalias, disjoints) => "RDFNoder: " + relation.n.s + ", " + toString(relalias)
      case RDFBNoder(relation, relalias, disjoints) => "RDFBNoder: " + relation.n.s + ", " + toString(relalias)
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
	val state_subjJoin = R2RState(state_postSubj.joins + InnerJoin(AliasedResource(rel,relalias)), state_postSubj.varmap, state_postSubj.exprs)

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
	      val state_t = R2RState(state_subjJoin.joins + InnerJoin(AliasedResource(fkrel,oRelAlias)),
				     state_subjJoin.varmap,
				     state_subjJoin.exprs + RelationalExpressionEq(PrimaryExpressionAttr(fkaliasattr),PrimaryExpressionAttr(objattr)))

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

      case TableConjunction(list) =>
	/* Examine each triple, updating the compilation state. */
	list.foldLeft(Set[Var]())((x, y) => x ++ findVars(y))

      case OptionalGraphPattern(gp2) =>
	/* Examine each triple, updating the compilation state. */
	findVars(gp2)

      case x => error("no code to handle " + x)
    }
  }

  def varToAttribute(varmap:Map[Var, SQL2RDFValueMapper], vvar:Var):RelAliasAttribute = {
    varmap(vvar) match {
      case IntMapper(relalias, disjoints) => relalias
      case StringMapper(relalias, disjoints) => relalias
      case DateMapper(relalias, disjoints) => relalias
      case RDFNoder(relation, relalias, disjoints) => relalias
      case RDFBNoder(relation, relalias, disjoints) => relalias
    }
  }

  def varToAttributeDisjoints(varmap:Map[Var, SQL2RDFValueMapper], vvar:Var):Set[RelationalExpressionNe] = {
    varmap(vvar) match {
      case IntMapper(relalias, disjoints) => disjoints
      case StringMapper(relalias, disjoints) => disjoints
      case DateMapper(relalias, disjoints) => disjoints
      case RDFNoder(relation, relalias, disjoints) => disjoints
      case RDFBNoder(relation, relalias, disjoints) => disjoints
    }
  }

  def varToConcat(varmap:Map[Var, SQL2RDFValueMapper], vvar:Var, stem:StemURI):Expression = {
    varmap(vvar) match {
      case IntMapper(relalias, _) => PrimaryExpressionAttr(relalias)
      case StringMapper(relalias, _) => 
	Concat(List(PrimaryExpressionTyped(SQLDatatype("String"),Name("\\\"")),
		    PrimaryExpressionAttr(relalias),
		    PrimaryExpressionTyped(SQLDatatype("String"),Name("\\\"^^<http://www.w3.org/2001/XMLSchema#string>"))))
      case DateMapper(relalias, _) => PrimaryExpressionAttr(relalias)
      case RDFNoder(relation, relalias, _) => 
	Concat(List(PrimaryExpressionTyped(SQLDatatype("String"),Name(stem.s)),
		    PrimaryExpressionTyped(SQLDatatype("String"),relation.n),
		    PrimaryExpressionTyped(SQLDatatype("String"),Name("/")),
		    PrimaryExpressionTyped(SQLDatatype("String"),relalias.attribute.n),
		    PrimaryExpressionTyped(SQLDatatype("String"),Name(".")),
		    PrimaryExpressionAttr(relalias),
		    PrimaryExpressionTyped(SQLDatatype("String"),Name("#record"))))
      case RDFBNoder(relation, relalias, _) => 
	Concat(List(PrimaryExpressionTyped(SQLDatatype("String"),Name("_:")),
		    PrimaryExpressionTyped(SQLDatatype("String"),relation.n),
		    PrimaryExpressionTyped(SQLDatatype("String"),Name(".")),
		    PrimaryExpressionTyped(SQLDatatype("String"),relalias.attribute.n),
		    PrimaryExpressionTyped(SQLDatatype("String"),Name(".")),
		    PrimaryExpressionAttr(relalias)))
    }
    
  }

  def filter2expr(varmap:Map[Var, SQL2RDFValueMapper], f:SparqlPrimaryExpression):RelationalExpression = {
    val (lTerm:Term, rTerm:Term, sqlexpr) = f match { // sqlexpr::((RelAliasAttribute,PrimaryExpressionAttr)=>RelationalExpression)
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
	    PrimaryExpressionAttr(varToAttribute(varmap, v))
	  }
	  case TermLit(lit) => null // :SparqlLiteral => PrimaryExpressionTyped(SQLDatatype, lit.n)
	}
	sqlexpr(PrimaryExpressionAttr(l), r)
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
	val nullExprs = findVars(gp) map (vvar => RelationalExpressionNotNull(PrimaryExpressionAttr(varToAttribute(state2.varmap, vvar))))
	R2RState(state2.joins, state2.varmap, state2.exprs ++ nullExprs)
      }
      case TableConjunction(list) => {
	list.foldLeft(state)((incState,s) => mapGraphPattern(db, incState, s, pk, enforeForeignKeys))
      }
      case TableDisjunction(list) => {
	val unionAlias = RelAlias(Name("R_union" + state.joins.size))
	val initDisjoints:Set[Select] = Set()
	val emptyState = R2RState(
	  AddOrderedSet[Join](), 
	  Map[Var, SQL2RDFValueMapper](), 
	  Set[Expression]()
	)
	val unionVars = list.foldLeft(Set[Var]())((mySet,disjoint) => mySet ++ findVars(disjoint)).toList
	val (state2, disjoints, count) = list.foldLeft((state, initDisjoints, 0))((incPair,disjoint) => {
	  val (outerState, outerDisjoints, no) = incPair
	  val disjointState = mapGraphPattern(db, emptyState, disjoint, pk, enforeForeignKeys)
	  val disjointVars = findVars(disjoint)
	  val disjointNo = NamedAttribute(PrimaryExpressionTyped(SQLDatatype.INTEGER,Name("" + no)), AttrAlias(Name("_DISJOINT_")))
	  val disjointNoAliasAttr = RelAliasAttribute(unionAlias, Attribute(Name("_DISJOINT_")))
	  val disjointCond = RelationalExpressionNe(PrimaryExpressionAttr(disjointNoAliasAttr), PrimaryExpressionTyped(SQLDatatype.INTEGER,Name("" + no)))

	  val attrlist:Set[NamedAttribute] = unionVars.foldLeft(Set(disjointNo))((attrs, v) => {
	    val attrOrNull = if (disjointState.varmap.contains(v)) varToAttribute(disjointState.varmap, v) else ConstNULL()
	    attrs ++ Set(NamedAttribute(attrOrNull, AttrAlias(Name("A_" + v.s))))
	  })

	  val subselect = Select(
	    AttributeList(attrlist),
	    TableList(disjointState.joins),
	    disjointState.exprs.size match {
	      case 0 => None
	      case 1 => Some(disjointState.exprs.toList(0))
	      case _ => Some(ExprConjunction(disjointState.exprs))
	    }
	  )
	  val outerState2 = disjointVars.foldLeft(outerState)((myState, v) => {
	    val varAliasAttr = RelAliasAttribute(unionAlias, Attribute(Name("A_" + v.s)))
	    if (myState.varmap.contains(v)) {
	      /* The variable has already been bound. */
	      val newMap:Map[Var, SQL2RDFValueMapper] = if (varToAttribute(myState.varmap, v) == varAliasAttr) {
		/* Same var was bound in an earlier disjoint. */
		val oldDisjoints = varToAttributeDisjoints(myState.varmap, v)
		// myState
		Map(v -> { disjointState.varmap(v) match {
		  case IntMapper(_, _)      => IntMapper(varAliasAttr, oldDisjoints + disjointCond)
		  case StringMapper(_, _)   => StringMapper(varAliasAttr, oldDisjoints + disjointCond)
		  case DateMapper(_, _)     => DateMapper(varAliasAttr, oldDisjoints + disjointCond)
		  case RDFNoder(rel, _, _)  => RDFNoder(rel, varAliasAttr, oldDisjoints + disjointCond)
		  case RDFBNoder(rel, _, _) => RDFBNoder(rel, varAliasAttr, oldDisjoints + disjointCond)
		} } )
	      } else
		Map()
	      val newConstraints =
		if (varToAttribute(outerState.varmap, v) != varAliasAttr) {
		  /* Constraint against binding from earlier GP. */
		  val constraint = RelationalExpressionEq(PrimaryExpressionAttr(varToAttribute(outerState.varmap, v)), PrimaryExpressionAttr(varAliasAttr))
		  if (varToAttributeDisjoints(outerState.varmap, v).size > 0)
		    // (union0._DISJOINT_ != 0 AND union1._DISJOINT_ != 2) OR union0.x=union1.x
		    varToAttributeDisjoints(outerState.varmap, v) map ((d) => ExprDisjunction(Set(ExprConjunction(Set(d, disjointCond)), constraint)))
		  else
		    Set(ExprDisjunction(Set(disjointCond, constraint)))
		} else {
		  Set()
		}
	      R2RState(myState.joins, myState.varmap ++ newMap, myState.exprs ++ newConstraints)
	    } else {
	      /* This variable is new to the outer context. */
	      val mapper:SQL2RDFValueMapper = disjointState.varmap(v) match {
		case IntMapper(_, _)      => IntMapper(varAliasAttr, Set(disjointCond))
		case StringMapper(_, _)   => StringMapper(varAliasAttr, Set(disjointCond))
		case DateMapper(_, _)   => DateMapper(varAliasAttr, Set(disjointCond))
		case RDFNoder(rel, _, _)  => RDFNoder(rel, varAliasAttr, Set(disjointCond))
		case RDFBNoder(rel, _, _) => RDFBNoder(rel, varAliasAttr, Set(disjointCond))
	      }
	      R2RState(myState.joins, myState.varmap + (v -> mapper), myState.exprs)
	    }
	  })
	  (outerState2, outerDisjoints ++ Set(subselect), no+1)
	})
	val subselect = Subselect(Union(disjoints))
	R2RState(state.joins + InnerJoin(AliasedResource(subselect,unionAlias)), state2.varmap, state2.exprs)
      }
      case OptionalGraphPattern(gp) => {
      	val leftJoinAlias = RelAlias(Name("R_opt" + state.joins.size))
      	val initDisjoints:Set[Select] = Set()
      	val emptyState = R2RState(
      	  AddOrderedSet[Join](), 
      	  Map[Var, SQL2RDFValueMapper](), 
      	  Set[Expression]()
      	)
      	val optionalState = mapGraphPattern(db, emptyState, gp, pk, enforeForeignKeys)
      	val optionalVars = findVars(gp)

      	val leftJoinVars = findVars(gp).toList
      	val attrlist:Set[NamedAttribute] = leftJoinVars.foldLeft(Set[NamedAttribute]())((attrs, v) =>
      	  attrs ++ Set(NamedAttribute(varToAttribute(optionalState.varmap, v), AttrAlias(Name("A_" + v.s))))
      	)

      	val subselect = Select(
      	  AttributeList(attrlist),
      	  TableList(optionalState.joins),
      	  optionalState.exprs.size match {
      	    case 0 => None
      	    case 1 => Some(optionalState.exprs.toList(0))
      	    case _ => Some(ExprConjunction(optionalState.exprs))
      	  }
      	)

      	val outerState2 = optionalVars.foldLeft(R2RState(state.joins, state.varmap, Set[Expression]()))((myState, v) => {
      	  val varAliasAttr = RelAliasAttribute(leftJoinAlias, Attribute(Name("A_" + v.s)))
      	  if (myState.varmap.contains(v)) {
      	    /* The variable has already been bound. */
      	    val newMap:Map[Var, SQL2RDFValueMapper] = if (varToAttribute(myState.varmap, v) == varAliasAttr) {
      	      /* Same var was already bound. */
	      error("Variable " + v + " already bound to " + varAliasAttr)
      	    } else
      	      Map()
      	    val newConstraints = {
      	      /* Constraint against binding from earlier GP. */
      	      val constraint = RelationalExpressionEq(PrimaryExpressionAttr(varToAttribute(state.varmap, v)), PrimaryExpressionAttr(varAliasAttr))
      	      if (varToAttributeDisjoints(state.varmap, v).size > 0)
      		// (leftJoin0._DISJOINT_ != 0 AND leftJoin1._DISJOINT_ != 2) OR leftJoin0.x=leftJoin1.x
      		varToAttributeDisjoints(state.varmap, v) map ((d) => ExprDisjunction(Set(d, constraint)))
      	      else
      		Set(constraint)
      	    }
      	    R2RState(myState.joins, myState.varmap ++ newMap, myState.exprs ++ newConstraints)
      	  } else {
      	    /* This variable is new to the outer context. */
      	    val mapper:SQL2RDFValueMapper = optionalState.varmap(v) match {
      	      case IntMapper(_, _)      => IntMapper(varAliasAttr, Set())
      	      case StringMapper(_, _)   => StringMapper(varAliasAttr, Set())
      	      case DateMapper(_, _)   => DateMapper(varAliasAttr, Set())
      	      case RDFNoder(rel, _, _)  => RDFNoder(rel, varAliasAttr, Set())
      	      case RDFBNoder(rel, _, _) => RDFBNoder(rel, varAliasAttr, Set())
      	    }
      	    R2RState(myState.joins, myState.varmap + (v -> mapper), myState.exprs)
      	  }
      	})
      	val join = LeftOuterJoin(AliasedResource(Subselect(subselect), leftJoinAlias), 
      	  outerState2.exprs.size match {
      	    case 0 => error ("Nested GP has no variables shared with its context; cowaredly refusing to join ON 1.")
      	    case 1 => outerState2.exprs.toList(0)
      	    case _ => ExprConjunction(outerState2.exprs)
      	  }
			       )
      	R2RState(state.joins + join, outerState2.varmap, state.exprs)
      }
      case x => error("no code to handle " + x)
    }
  }

  def apply (db:DatabaseDesc, sparql:SparqlSelect, stem:StemURI, pk:PrimaryKey, enforeForeignKeys:Boolean, concat:Boolean) : Select = {
    val SparqlSelect(attrs, triples) = sparql

    /* Create an object to hold our compilation state. */
    val initState = R2RState(
      AddOrderedSet[Join](), 
      Map[Var, SQL2RDFValueMapper](), 
      Set[Expression]()
    )

    val r2rState = mapGraphPattern(db, initState, sparql.gp, pk, enforeForeignKeys)

    /* Select the attributes corresponding to the variables
     * in the SPARQL SELECT.  */
    val attrlist:Set[NamedAttribute] = attrs.attributelist.foldLeft(Set[NamedAttribute]())((attrs, vvar) => 
      attrs + NamedAttribute({
	if (concat) varToConcat(r2rState.varmap, vvar, stem)
	else varToAttribute(r2rState.varmap, vvar)
      } , AttrAlias(Name("A_" + vvar.s))
      ))

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
