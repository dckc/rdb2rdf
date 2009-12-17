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
  case class R2RState(joined:Set[RelAlias], allVars:List[Var], inConstraint:Set[Var], joins:Set[AliasedResource], varmap:Map[Var, SQL2RDFValueMapper], exprs:Set[PrimaryExpression])

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

  def uriConstraint(constrainMe:RelAliasAttribute, u:ObjUri):PrimaryExpression = {
    // println("equiv+= " + toString(constrainMe) + "=" + value)
    PrimaryExpressionEq(constrainMe,RValueTyped(SQLDatatype.INTEGER,Name(u.v.s)))
  }

  def literalConstraint(constrainMe:RelAliasAttribute, lit:SparqlLiteral, dt:SQLDatatype):PrimaryExpression = {
    // println("equiv+= " + toString(attr) + "=" + lit)
    PrimaryExpressionEq(constrainMe,RValueTyped(dt,Name(lit.lit.lexicalForm)))
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
	s match {
	  case SUri(u) => exprs += uriConstraint(subjattr, u)
	  case SVar(v) => {
	    val binding:SQL2RDFValueMapper = varConstraint(subjattr, v, db, rel)
	    varmap += v -> binding
	    List()
	  }
	}
	joined contains(relalias) match {
	  case false => {
	    //joins = joins ::: List(Join(AliasedResource(rel,relalias), sconstraint))
	    joined += relalias
	    joins += AliasedResource(rel,relalias)
	  }
	  case true =>
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
	      case OLit(l) => {
		exprs += joinconstraint
		exprs += literalConstraint(fkaliasattr, l, dt)
	      }

	      case OUri(u) => {
		exprs += joinconstraint
		exprs += uriConstraint(fkaliasattr, u)
	      }

	      case OVar(v) => {
		val binding = varConstraint(fkaliasattr, v, db, fkrel)
		varmap += v -> binding
		exprs += joinconstraint
	      }
	    }

	    joined contains(oRelAlias) match {
	      case false => {

		joins += AliasedResource(fkrel,oRelAlias)
		joined = joined + oRelAlias
	      }
	      case true => {
	      }
	    }
	  }
	  case Value(dt) => {
	    o match {
	      case OLit(l) => exprs += literalConstraint(objattr, l, dt)
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

  def varToAttribute(varmap:Map[Var, SQL2RDFValueMapper], vvar:Var):RelAliasAttribute = {
    varmap(vvar) match {
      case StringMapper(relalias) => relalias
      case IntMapper(relalias) => relalias
      case RDFNoder(relation, relalias) => relalias
    }
  }

  def filter(exprsP:Set[PrimaryExpression], inConstraintP:Set[Var], varmap:Map[Var, SQL2RDFValueMapper], f:SparqlPrimaryExpression):Tuple2[Set[PrimaryExpression], Set[Var]] = {
    var exprs = exprsP
    var inConstraint = inConstraintP
    // var f = SparqlPrimaryExpressionEq
    // val e:SparqlPrimaryExpression = f(SparqlTermExpression(Term(TermVar(Var("a")))), SparqlTermExpression(Term(TermVar(Var("a")))))
    val tup:Tuple3[Term, Term, String] = f match {
      case SparqlPrimaryExpressionEq(l, r) => (l.term, r.term, "==")
      case SparqlPrimaryExpressionLt(l, r) => (l.term, r.term, "<")
      }
    tup._1 match {
      case TermUri(obj) => error("only SPARQL PrimaryExpressions with a variable on the left have been implemented: punting on " + f)
      case TermVar(v) => { // :Var
	inConstraint += v
	val l = varToAttribute(varmap, v)
	val r = tup._2 match {
	  case TermUri(obj) => null // :ObjUri
	  case TermVar(v) => { // :Var
	    inConstraint += v
	    RValueAttr(varToAttribute(varmap, v))
	  }
	  case TermLit(lit) => null // :SparqlLiteral => RValueTyped(SQLDatatype, lit.n)
	}
	tup._3 match {
	  case "==" => exprs += PrimaryExpressionEq(l, r)
	  case _ => exprs += PrimaryExpressionLt(l, r)
	}
	(exprs, inConstraint)
      }
      case TermLit(lit) => error("only SPARQL PrimaryExpressions with a variable on the left have been implemented: punting on " + f)
    } 
  }

  def nullGuard(exprs:Set[PrimaryExpression], inConstraint:Set[Var], varmap:Map[Var, SQL2RDFValueMapper], vvar:Var):Set[PrimaryExpression] = {
    var ret = exprs
    inConstraint contains(vvar) match {
      case false => {
	val mapper:SQL2RDFValueMapper = varmap(vvar)
	val aattr = mapper match {
	  case StringMapper(relalias) => relalias
	  case IntMapper(relalias) => relalias
	  case RDFNoder(relation, relalias) => relalias
	}
	ret += PrimaryExpressionNotNull(aattr)
      }
      case true => 
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
      Set[AliasedResource](), 
      Map[Var, SQL2RDFValueMapper](), 
      Set[PrimaryExpression]()
    )

    /* Examine each triple, updating the compilation state. */
    triples.triplepatterns.foreach(s => r2rState = acc(db, r2rState, s, pk))

    /* Select the attributes corresponding to the variables
     * in the SPARQL SELECT.  */
    var attrlist:List[NamedAttribute] = List()
    attrs.attributelist.foreach(vvar => attrlist = attrlist ::: List(
      NamedAttribute(varToAttribute(r2rState.varmap, vvar), AttrAlias(Name("A_" + vvar.s)))
    ))

    var exprs:Set[PrimaryExpression] = r2rState.exprs
    var inConstraint:Set[Var] = r2rState.inConstraint

    /* Add constraints for all the FILTERS */
    triples.filter.conjuncts.foreach(f => {
	val pair = filter(exprs, inConstraint, r2rState.varmap, f)
	exprs = pair._1
	inConstraint = pair._2
      })

    /* Add null guards for attributes associated with variables which
     * are not optional and have not been used in constraints. */
    r2rState.allVars.foreach(s => exprs = nullGuard(exprs, inConstraint, r2rState.varmap, s))

    /* Construct the generated query as an abstract syntax. */
    Select(
      AttributeList(attrlist),
      TableList(r2rState.joins),
      Expression(exprs)
    )
  }
}
