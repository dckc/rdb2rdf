package w3c.sw.rdb2rdf

import scala.util.parsing.combinator._
import java.net.URI
import w3c.sw.sql
import w3c.sw.sparql
import w3c.sw.util

case class StemURI(s:String)
case class PrimaryKey(attr:sql.Attribute)

sealed abstract class Binding
case class RDFNode(relaliasattr:sql.RelAliasAttribute) extends Binding
case class Str(relaliasattr:sql.RelAliasAttribute) extends Binding
case class Int(relaliasattr:sql.RelAliasAttribute) extends Binding
case class Enum(relaliasattr:sql.RelAliasAttribute) extends Binding

object RDB2RDF {
  case class R2RState(joins:util.AddOrderedSet[sql.Join], varmap:Map[sparql.Var, SQL2RDFValueMapper], exprs:Set[sql.Expression])

  sealed abstract class SQL2RDFValueMapper(relaliasattr:sql.RelAliasAttribute, disjoints:Set[sql.RelationalExpressionNe])
  case class IntMapper(relaliasattr:sql.RelAliasAttribute, disjoints:Set[sql.RelationalExpressionNe]) extends SQL2RDFValueMapper(relaliasattr, disjoints)
  case class StringMapper(relaliasattr:sql.RelAliasAttribute, disjoints:Set[sql.RelationalExpressionNe]) extends SQL2RDFValueMapper(relaliasattr, disjoints)
  case class DateMapper(relaliasattr:sql.RelAliasAttribute, disjoints:Set[sql.RelationalExpressionNe]) extends SQL2RDFValueMapper(relaliasattr, disjoints)
  case class RDFNoder(relation:sql.Relation, relaliasattr:sql.RelAliasAttribute, disjoints:Set[sql.RelationalExpressionNe]) extends SQL2RDFValueMapper(relaliasattr, disjoints)
  case class RDFBNoder(relation:sql.Relation, relaliasattr:sql.RelAliasAttribute, disjoints:Set[sql.RelationalExpressionNe]) extends SQL2RDFValueMapper(relaliasattr, disjoints)

  def relAliasFromS(s:sparql.S):sql.RelAlias = {
    s match {
      case sparql.SUri(ob) => relAliasFromNode(ob)
      case sparql.SVar(v) => relAliasFromVar(v)
    }
  }

  def relAliasFromO(o:sparql.O):sql.RelAlias = {
    o match {
      case sparql.OUri(ob) => relAliasFromNode(ob)
      case sparql.OVar(v) => relAliasFromVar(v)
      case sparql.OLit(l) => relAliasFromLiteral(l)
    }
  }

  def relAliasFromNode(u:sparql.ObjUri):sql.RelAlias = {
    val sparql.ObjUri(stem, rel, sparql.Attr(a), sparql.CellValue(v)) = u
    sql.RelAlias(sql.Name("R_" + a + v))
  }

  def relAliasFromLiteral(l:sparql.SparqlLiteral):sql.RelAlias = {
    sql.RelAlias(sql.Name("R_" + l.lit.lexicalForm))
  }

  def relAliasFromVar(vr:sparql.Var):sql.RelAlias = {
    val sparql.Var(v) = vr
    sql.RelAlias(sql.Name("R_" + v))
  }

  def uriConstraint(state:R2RState, constrainMe:sql.RelAliasAttribute, u:sparql.ObjUri, enforeForeignKeys:Boolean):R2RState = {
    // println("equiv+= " + toString(constrainMe) + "=" + value)
    //R2RState(state.joins, state.varmap, state.exprs + RelationalExpressionEq(constrainMe,PrimaryExpressionTyped(SQLDatatype.INTEGER,Name(u.v.s))))
    val relvar = if (enforeForeignKeys) sql.RelAliasAttribute(constrainMe.relalias, sql.Attribute(sql.Name(u.attr.s))) else constrainMe
    R2RState(state.joins, state.varmap, state.exprs + sql.RelationalExpressionEq(sql.PrimaryExpressionAttr(relvar),sql.PrimaryExpressionTyped(sql.SQLDatatype.INTEGER,sql.Name(u.v.s))))
  }

  def literalConstraint(state:R2RState, constrainMe:sql.RelAliasAttribute, lit:sparql.SparqlLiteral, dt:sql.SQLDatatype):R2RState = {
    // println("equiv+= " + toString(attr) + "=" + lit)
    R2RState(state.joins, state.varmap, state.exprs + sql.RelationalExpressionEq(sql.PrimaryExpressionAttr(constrainMe),sql.PrimaryExpressionTyped(dt,sql.Name(lit.lit.lexicalForm))))    
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
  def varConstraint(state:R2RState, constrainMe:sql.RelAliasAttribute, v:sparql.Var, db:sql.DatabaseDesc, rel:sql.Relation):R2RState = {
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
	val constraint = sql.RelationalExpressionEq(sql.PrimaryExpressionAttr(varToAttribute(state.varmap, v)), sql.PrimaryExpressionAttr(constrainMe))
	R2RState(state.joins, state.varmap, 
		 if (varToAttributeDisjoints(state.varmap, v).size > 0) {
		   state.exprs ++ {varToAttributeDisjoints(state.varmap, v) map ((d) => sql.ExprDisjunction(Set(d, constraint)))}
		 } else
		   state.exprs + constraint
	       )
      }
    } else {
      /* This is a new variable. */
      val binding = reldesc.primarykey match {
	case Some(sql.Attribute(constrainMe.attribute.n)) => 
	  RDFNoder(rel, constrainMe, Set())
	case _ => {
	  // e.g. sql.Attribute(sql.Name("id")) or None
	  if (reldesc.attributes.contains(constrainMe.attribute)) {
	    reldesc.attributes(constrainMe.attribute) match {
	      case sql.ForeignKey(fkrel, fkattr) =>
		RDFNoder(rel, constrainMe, Set())
	      case sql.Value(sql.SQLDatatype("Int")) =>
		IntMapper(constrainMe, Set())
	      case sql.Value(sql.SQLDatatype("String")) =>
		StringMapper(constrainMe, Set())
	      case sql.Value(sql.SQLDatatype("Date")) =>
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

  def toString(relaliasattr:sql.RelAliasAttribute) : String = {
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

  def bindOnPredicate(db:sql.DatabaseDesc, stateP:R2RState, triple:sparql.TriplePattern, pk:PrimaryKey, enforceForeignKeys:Boolean):R2RState = {
    val sparql.TriplePattern(s, p, o) = triple
    p match {
      case sparql.PVar(v) => error("variable predicates require tedious enumeration; too tedious for me.")
      case sparql.PUri(stem, spRel, spAttr) => {
	/* Attributes that come from the predicate: */
	val rel = sql.Relation(sql.Name(spRel.s))
	val attr = sql.Attribute(sql.Name(spAttr.s))
	val relalias = relAliasFromS(s)

	/* Attributes that come from the subject: */
	val subjattr = sql.RelAliasAttribute(relalias, pk.attr)
	val objattr = sql.RelAliasAttribute(relalias, attr)
	val state_postSubj = s match {
	  case sparql.SUri(u) => uriConstraint(stateP, subjattr, u, true)
	  case sparql.SVar(v) => varConstraint(stateP, subjattr, v, db, rel)
	}
	val state_subjJoin = R2RState(state_postSubj.joins + sql.InnerJoin(sql.AliasedResource(rel,relalias)), state_postSubj.varmap, state_postSubj.exprs)

	val (targetattr, targetrel, dt, state_fkeys:R2RState) = db.relationdescs(rel).attributes(attr) match {
	  case sql.ForeignKey(fkrel, fkattr) => {
	    val fkdt = db.relationdescs(fkrel).attributes(fkattr) match {
	      case sql.ForeignKey(dfkrel, dfkattr) => error("foreign key " + rel.n + "." + attr.n + 
							"->" + fkrel.n + "." + fkattr.n + 
							"->" + dfkrel.n + "." + dfkattr.n)
	      case sql.Value(x) => x
	    }
	    if (enforceForeignKeys) {
	      val oRelAlias = relAliasFromO(o)
	      val fkaliasattr = sql.RelAliasAttribute(oRelAlias, fkattr)
	      val state_t = R2RState(state_subjJoin.joins + sql.InnerJoin(sql.AliasedResource(fkrel,oRelAlias)),
				     state_subjJoin.varmap,
				     state_subjJoin.exprs + sql.RelationalExpressionEq(sql.PrimaryExpressionAttr(fkaliasattr),sql.PrimaryExpressionAttr(objattr)))

	      (fkaliasattr, fkrel, fkdt, state_t)
	    } else {
	      (objattr, rel, fkdt, state_subjJoin)
	    }
	  }
	  case sql.Value(dt) => (objattr, rel, dt, state_subjJoin)
	}
	o match {
	  case sparql.OLit(l) => literalConstraint(state_fkeys, targetattr, l, dt)
	  case sparql.OUri(u) => uriConstraint    (state_fkeys, targetattr, u, enforceForeignKeys)
	  case sparql.OVar(v) => varConstraint    (state_fkeys, targetattr, v, db, targetrel)
	}
      }
    }
  }

  def findVars(triple:sparql.TriplePattern):Set[sparql.Var] = {
    val sparql.TriplePattern(s, p, o) = triple
    val varS:Set[sparql.Var] = s match {
      case sparql.SVar(v) => Set(v)
      case _              => Set()
    }
    val varO:Set[sparql.Var] = o match {
      case sparql.OVar(v) => Set(v)
      case _              => Set()
    }
    varS ++ varO
  }

  def findVars(gp:sparql.GraphPattern):Set[sparql.Var] = {
    gp match {
      case sparql.TableFilter(gp2:sparql.GraphPattern, expr:sparql.SparqlExpression) =>
	findVars(gp2)

      case sparql.TriplesBlock(triplepatterns) =>
	/* Examine each triple, updating the compilation state. */
	triplepatterns.foldLeft(Set[sparql.Var]())((x, y) => x ++ findVars(y))

      case sparql.TableConjunction(list) =>
	/* Examine each triple, updating the compilation state. */
	list.foldLeft(Set[sparql.Var]())((x, y) => x ++ findVars(y))

      case sparql.OptionalGraphPattern(gp2) =>
	/* Examine each triple, updating the compilation state. */
	findVars(gp2)

      case x => error("no code to handle " + x)
    }
  }

  def varToAttribute(varmap:Map[sparql.Var, SQL2RDFValueMapper], vvar:sparql.Var):sql.RelAliasAttribute = {
    varmap(vvar) match {
      case IntMapper(relalias, disjoints) => relalias
      case StringMapper(relalias, disjoints) => relalias
      case DateMapper(relalias, disjoints) => relalias
      case RDFNoder(relation, relalias, disjoints) => relalias
      case RDFBNoder(relation, relalias, disjoints) => relalias
    }
  }

  def varToAttributeDisjoints(varmap:Map[sparql.Var, SQL2RDFValueMapper], vvar:sparql.Var):Set[sql.RelationalExpressionNe] = {
    varmap(vvar) match {
      case IntMapper(relalias, disjoints) => disjoints
      case StringMapper(relalias, disjoints) => disjoints
      case DateMapper(relalias, disjoints) => disjoints
      case RDFNoder(relation, relalias, disjoints) => disjoints
      case RDFBNoder(relation, relalias, disjoints) => disjoints
    }
  }

  def varToConcat(varmap:Map[sparql.Var, SQL2RDFValueMapper], vvar:sparql.Var, stem:StemURI):sql.Expression = {
    varmap(vvar) match {
      case IntMapper(relalias, _) => sql.PrimaryExpressionAttr(relalias)
      case StringMapper(relalias, _) => 
	sql.Concat(List(sql.PrimaryExpressionTyped(sql.SQLDatatype("String"),sql.Name("\\\"")),
		    sql.PrimaryExpressionAttr(relalias),
		    sql.PrimaryExpressionTyped(sql.SQLDatatype("String"),sql.Name("\\\"^^<http://www.w3.org/2001/XMLSchema#string>"))))
      case DateMapper(relalias, _) => sql.PrimaryExpressionAttr(relalias)
      case RDFNoder(relation, relalias, _) => 
	sql.Concat(List(sql.PrimaryExpressionTyped(sql.SQLDatatype("String"),sql.Name(stem.s)),
		    sql.PrimaryExpressionTyped(sql.SQLDatatype("String"),relation.n),
		    sql.PrimaryExpressionTyped(sql.SQLDatatype("String"),sql.Name("/")),
		    sql.PrimaryExpressionTyped(sql.SQLDatatype("String"),relalias.attribute.n),
		    sql.PrimaryExpressionTyped(sql.SQLDatatype("String"),sql.Name(".")),
		    sql.PrimaryExpressionAttr(relalias),
		    sql.PrimaryExpressionTyped(sql.SQLDatatype("String"),sql.Name("#record"))))
      case RDFBNoder(relation, relalias, _) => 
	sql.Concat(List(sql.PrimaryExpressionTyped(sql.SQLDatatype("String"),sql.Name("_:")),
		    sql.PrimaryExpressionTyped(sql.SQLDatatype("String"),relation.n),
		    sql.PrimaryExpressionTyped(sql.SQLDatatype("String"),sql.Name(".")),
		    sql.PrimaryExpressionTyped(sql.SQLDatatype("String"),relalias.attribute.n),
		    sql.PrimaryExpressionTyped(sql.SQLDatatype("String"),sql.Name(".")),
		    sql.PrimaryExpressionAttr(relalias)))
    }
    
  }

  def filter2expr(varmap:Map[sparql.Var, SQL2RDFValueMapper], f:sparql.SparqlPrimaryExpression):sql.RelationalExpression = {
    val (lTerm:sparql.Term, rTerm:sparql.Term, sqlexpr) = f match { // sqlexpr::((sql.RelAliasAttribute,sql.PrimaryExpressionAttr)=>sql.RelationalExpression)
      case sparql.SparqlPrimaryExpressionEq(l, r) => (l.term, r.term, sql.RelationalExpressionEq(_,_))
      case sparql.SparqlPrimaryExpressionLt(l, r) => (l.term, r.term, sql.RelationalExpressionLt(_,_))
    }
// sql.RelationalExpressionEq(_,_) === (x,y) => PrymaryExpressionEq(x,y)
    lTerm match {
      // does not handle FILTER (<x> = ?v)
      case sparql.TermUri(obj) => error("only SPARQL PrimaryExpressions with a variable on the left have been implemented: punting on " + f)
      // FILTER (?v = <x> && ?v = ?x && ?v = 7)
      case sparql.TermVar(v) => { // :sparql.Var
	val l = varToAttribute(varmap, v)
	val r = rTerm match {
	  case sparql.TermUri(obj) => null // :sparql.ObjUri
	  case sparql.TermVar(v) => { // :sparql.Var
	    sql.PrimaryExpressionAttr(varToAttribute(varmap, v))
	  }
	  case sparql.TermLit(lit) => null // :sparql.SparqlLiteral => sql.PrimaryExpressionTyped(sql.SQLDatatype, lit.n)
	}
	sqlexpr(sql.PrimaryExpressionAttr(l), r)
      }
      // does not handle FILTER (7 = ?v)
      case sparql.TermLit(lit) => error("only SPARQL PrimaryExpressions with a variable on the left have been implemented: punting on " + f)
    }
  }

  def mapGraphPattern(db:sql.DatabaseDesc, state:R2RState, gp:sparql.GraphPattern, pk:PrimaryKey, enforceForeignKeys:Boolean):R2RState = {
    gp match {
      case sparql.TableFilter(gp2:sparql.GraphPattern, expr:sparql.SparqlExpression) => {
	val state2 = mapGraphPattern(db, state, gp2, pk, enforceForeignKeys)

	/* Add constraints for all the FILTERS */
	val filterExprs:Set[sql.RelationalExpression] =
	  expr.conjuncts.toSet map ((x:sparql.SparqlPrimaryExpression) => filter2expr(state2.varmap, x))

	R2RState(state2.joins, state2.varmap, state2.exprs ++ filterExprs)
      }
      case sparql.TriplesBlock(triplepatterns) => {
	/* Examine each triple, updating the compilation state. */
	val state2 = triplepatterns.foldLeft(state)((incState,s) => bindOnPredicate(db, incState, s, pk, enforceForeignKeys))
	val nullExprs = findVars(gp) map (vvar => sql.RelationalExpressionNotNull(sql.PrimaryExpressionAttr(varToAttribute(state2.varmap, vvar))))
	R2RState(state2.joins, state2.varmap, state2.exprs ++ nullExprs)
      }
      case sparql.TableConjunction(list) => {
	list.foldLeft(state)((incState,s) => mapGraphPattern(db, incState, s, pk, enforceForeignKeys))
      }
      case sparql.TableDisjunction(list) => {
	val unionAlias = sql.RelAlias(sql.Name("R_union" + state.joins.size))
	val initDisjoints:Set[sql.Select] = Set()
	val emptyState = R2RState(
	  util.AddOrderedSet[sql.Join](), 
	  Map[sparql.Var, SQL2RDFValueMapper](), 
	  Set[sql.Expression]()
	)
	val unionVars = list.foldLeft(Set[sparql.Var]())((mySet,disjoint) => mySet ++ findVars(disjoint)).toList
	val (state2, disjoints, count) = list.foldLeft((state, initDisjoints, 0))((incPair,disjoint) => {
	  val (outerState, outerDisjoints, no) = incPair
	  val disjointState = mapGraphPattern(db, emptyState, disjoint, pk, enforceForeignKeys)
	  val disjointVars = findVars(disjoint)
	  val disjointNo = sql.NamedAttribute(sql.PrimaryExpressionTyped(sql.SQLDatatype.INTEGER,sql.Name("" + no)), sql.AttrAlias(sql.Name("_DISJOINT_")))
	  val disjointNoAliasAttr = sql.RelAliasAttribute(unionAlias, sql.Attribute(sql.Name("_DISJOINT_")))
	  val disjointCond = sql.RelationalExpressionNe(sql.PrimaryExpressionAttr(disjointNoAliasAttr), sql.PrimaryExpressionTyped(sql.SQLDatatype.INTEGER,sql.Name("" + no)))

	  val attrlist:Set[sql.NamedAttribute] = unionVars.foldLeft(Set(disjointNo))((attrs, v) => {
	    val attrOrNull = if (disjointState.varmap.contains(v)) varToAttribute(disjointState.varmap, v) else sql.ConstNULL()
	    attrs ++ Set(sql.NamedAttribute(attrOrNull, sql.AttrAlias(sql.Name("A_" + v.s))))
	  })

	  val subselect = sql.Select(
	    sql.AttributeList(attrlist),
	    sql.TableList(disjointState.joins),
	    disjointState.exprs.size match {
	      case 0 => None
	      case 1 => Some(disjointState.exprs.toList(0))
	      case _ => Some(sql.ExprConjunction(disjointState.exprs))
	    }
	  )
	  val outerState2 = disjointVars.foldLeft(outerState)((myState, v) => {
	    val varAliasAttr = sql.RelAliasAttribute(unionAlias, sql.Attribute(sql.Name("A_" + v.s)))
	    if (myState.varmap.contains(v)) {
	      /* The variable has already been bound. */
	      val newMap:Map[sparql.Var, SQL2RDFValueMapper] = if (varToAttribute(myState.varmap, v) == varAliasAttr) {
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
		  val constraint = sql.RelationalExpressionEq(sql.PrimaryExpressionAttr(varToAttribute(outerState.varmap, v)), sql.PrimaryExpressionAttr(varAliasAttr))
		  if (varToAttributeDisjoints(outerState.varmap, v).size > 0)
		    // (union0._DISJOINT_ != 0 AND union1._DISJOINT_ != 2) OR union0.x=union1.x
		    varToAttributeDisjoints(outerState.varmap, v) map ((d) => sql.ExprDisjunction(Set(sql.ExprConjunction(Set(d, disjointCond)), constraint)))
		  else
		    Set(sql.ExprDisjunction(Set(disjointCond, constraint)))
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
	val subselect = sql.Subselect(sql.Union(disjoints))
	R2RState(state.joins + sql.InnerJoin(sql.AliasedResource(subselect,unionAlias)), state2.varmap, state2.exprs)
      }
      case sparql.OptionalGraphPattern(gp) => {
      	val leftJoinAlias = sql.RelAlias(sql.Name("R_opt" + state.joins.size))
      	val initDisjoints:Set[sql.Select] = Set()
      	val emptyState = R2RState(
      	  util.AddOrderedSet[sql.Join](), 
      	  Map[sparql.Var, SQL2RDFValueMapper](), 
      	  Set[sql.Expression]()
      	)
      	val optionalState = mapGraphPattern(db, emptyState, gp, pk, enforceForeignKeys)
      	val optionalVars = findVars(gp)

      	val leftJoinVars = findVars(gp).toList
      	val attrlist:Set[sql.NamedAttribute] = leftJoinVars.foldLeft(Set[sql.NamedAttribute]())((attrs, v) =>
      	  attrs ++ Set(sql.NamedAttribute(varToAttribute(optionalState.varmap, v), sql.AttrAlias(sql.Name("A_" + v.s))))
      	)

      	val subselect = sql.Select(
      	  sql.AttributeList(attrlist),
      	  sql.TableList(optionalState.joins),
      	  optionalState.exprs.size match {
      	    case 0 => None
      	    case 1 => Some(optionalState.exprs.toList(0))
      	    case _ => Some(sql.ExprConjunction(optionalState.exprs))
      	  }
      	)

      	val outerState2 = optionalVars.foldLeft(R2RState(state.joins, state.varmap, Set[sql.Expression]()))((myState, v) => {
      	  val varAliasAttr = sql.RelAliasAttribute(leftJoinAlias, sql.Attribute(sql.Name("A_" + v.s)))
      	  if (myState.varmap.contains(v)) {
      	    /* The variable has already been bound. */
      	    val newMap:Map[sparql.Var, SQL2RDFValueMapper] = if (varToAttribute(myState.varmap, v) == varAliasAttr) {
      	      /* Same var was already bound. */
	      error("Variable " + v + " already bound to " + varAliasAttr)
      	    } else
      	      Map()
      	    val newConstraints = {
      	      /* Constraint against binding from earlier GP. */
      	      val constraint = sql.RelationalExpressionEq(sql.PrimaryExpressionAttr(varToAttribute(state.varmap, v)), sql.PrimaryExpressionAttr(varAliasAttr))
      	      if (varToAttributeDisjoints(state.varmap, v).size > 0)
      		// (leftJoin0._DISJOINT_ != 0 AND leftJoin1._DISJOINT_ != 2) OR leftJoin0.x=leftJoin1.x
      		varToAttributeDisjoints(state.varmap, v) map ((d) => sql.ExprDisjunction(Set(d, constraint)))
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
      	val join = sql.LeftOuterJoin(sql.AliasedResource(sql.Subselect(subselect), leftJoinAlias), 
      	  outerState2.exprs.size match {
      	    case 0 => error ("Nested GP has no variables shared with its context; cowaredly refusing to join ON 1.")
      	    case 1 => outerState2.exprs.toList(0)
      	    case _ => sql.ExprConjunction(outerState2.exprs)
      	  }
			       )
      	R2RState(state.joins + join, outerState2.varmap, state.exprs)
      }
      case x => error("no code to handle " + x)
    }
  }

  def apply (db:sql.DatabaseDesc, sparquery:sparql.SparqlSelect, stem:StemURI, pk:PrimaryKey, enforceForeignKeys:Boolean, concat:Boolean) : sql.Select = {
    val sparql.SparqlSelect(attrs, triples) = sparquery

    /* Create an object to hold our compilation state. */
    val initState = R2RState(
      util.AddOrderedSet[sql.Join](), 
      Map[sparql.Var, SQL2RDFValueMapper](), 
      Set[sql.Expression]()
    )

    val r2rState = mapGraphPattern(db, initState, sparquery.gp, pk, enforceForeignKeys)

    /* Select the attributes corresponding to the variables
     * in the SPARQL SELECT.  */
    val attrlist:Set[sql.NamedAttribute] = attrs.attributelist.foldLeft(Set[sql.NamedAttribute]())((attrs, vvar) => 
      attrs + sql.NamedAttribute({
	if (concat) varToConcat(r2rState.varmap, vvar, stem)
	else varToAttribute(r2rState.varmap, vvar)
      } , sql.AttrAlias(sql.Name("A_" + vvar.s))
      ))

    /* Construct the generated query as an abstract syntax. */
    sql.Select(
      sql.AttributeList(attrlist),
      sql.TableList(r2rState.joins),
      r2rState.exprs.size match {
	case 0 => None
	case 1 => Some(r2rState.exprs.toList(0))
	case _ => Some(sql.ExprConjunction(r2rState.exprs))
      }
    )
  }
}
