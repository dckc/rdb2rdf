package w3c.sw.sparql

import w3c.sw.rdf._
import org.scalatest.FunSuite
import java.net.URI

class SparqlTest extends FunSuite {

  test("parse a litstring") {
    val a = Sparql()
    val e = """
?emp      <http://hr.example/DB/Employee#lastName>   "bob"^^<http://www.w3.org/2001/XMLSchema#string>
"""
    val expected = TriplesBlock(List(TriplePattern(SVar(Var("emp")),PUri(Stem("http://hr.example/DB"),Rel("Employee"),Attr("lastName")),OLit(Literal(RDFLiteral("bob",Datatype(new URI("http://www.w3.org/2001/XMLSchema#string"))))))))
    assert(expected === (a.parseAll(a.triplesblock, e).get))
  }

  test("parse a litint") {
    val a = Sparql()
    val e = """
?emp      <http://hr.example/DB/Employee#age>   "21"^^<http://www.w3.org/2001/XMLSchema#integer>
"""
    val expected = TriplesBlock(List(TriplePattern(SVar(Var("emp")),PUri(Stem("http://hr.example/DB"),Rel("Employee"),Attr("age")),OLit(Literal(RDFLiteral("21",Datatype(new URI("http://www.w3.org/2001/XMLSchema#integer"))))))))
    assert(expected === (a.parseAll(a.triplesblock, e).get))
  }

  test("parse a triplesblock") {
    val a = Sparql()
    val e = """
?emp      <http://hr.example/DB/Employee#lastName>   ?empName .
?emp      <http://hr.example/DB/Employee#manager>    ?manager .
?manager  <http://hr.example/DB/Employee#lastName>   ?managName
"""
    val tps =
      TriplesBlock(
	List(
	  TriplePattern(
	    SVar(Var("emp")),
	    PUri(Stem("http://hr.example/DB"),Rel("Employee"),Attr("lastName")),
	    OVar(Var("empName"))),
	  TriplePattern(
	    SVar(Var("emp")),
	    PUri(Stem("http://hr.example/DB"),Rel("Employee"),Attr("manager")),
	    OVar(Var("manager"))),
	  TriplePattern(
	    SVar(Var("manager")),
	    PUri(Stem("http://hr.example/DB"),Rel("Employee"),Attr("lastName")),
	    OVar(Var("managName")))))
    assert(tps === a.parseAll(a.triplesblock, e).get)
  }

  // ?manBday < ?empBday && ?grandManBday < ?manBday
  test("SparqlTermExpression") {
    val a = Sparql()
    val e = """
?emp
"""
    val expected = SparqlTermExpression(TermVar(Var("emp")))
    assert(expected === (a.parseAll(a.value, e).get))
  }

  test("PrimaryExpressionEq") {
    val a = Sparql()
    val e = """
?emp<?emp
"""
    val expected = PrimaryExpressionLt(SparqlTermExpression(TermVar(Var("emp"))), SparqlTermExpression(TermVar(Var("emp"))))
    assert(expected === (a.parseAll(a.primaryexpression, e).get))
  }

  test("Expression") {
    val a = Sparql()
    val e = """
?manBday < ?empBday && ?grandManBday < ?manBday
"""
    val expected = Expression(List(
      PrimaryExpressionLt(SparqlTermExpression(TermVar(Var("manBday"))), SparqlTermExpression(TermVar(Var("empBday")))), 
      PrimaryExpressionLt(SparqlTermExpression(TermVar(Var("grandManBday"))), SparqlTermExpression(TermVar(Var("manBday"))))))
    assert(expected === (a.parseAll(a.expression, e).get))
  }

  test("FILTER") {
    val a = Sparql()
    val e = """
FILTER(?manBday < ?empBday && ?grandManBday < ?manBday)
"""
    val expected = Expression(List(
      PrimaryExpressionLt(SparqlTermExpression(TermVar(Var("manBday"))), SparqlTermExpression(TermVar(Var("empBday")))), 
      PrimaryExpressionLt(SparqlTermExpression(TermVar(Var("grandManBday"))), SparqlTermExpression(TermVar(Var("manBday"))))))
    assert(expected === (a.parseAll(a.filter, e).get))
  }

  test("SELECT") {
    val a = Sparql()
    val e = """
PREFIX empP : <http://hr.example/DB/Employee#>
SELECT ?empName ?manageName {
?emp      empP:lastName   ?empName
}
"""
    val tps =
      Select(
	SparqlAttributeList(List(Var("empName"), Var("manageName"))),
	TriplesBlock(
	  List(
	    TriplePattern(
	      SVar(Var("emp")),
		PUri(Stem("http://hr.example/DB"),Rel("Employee"),Attr("lastName")),
	      OVar(Var("empName"))))))
    assert(tps === a.parseAll(a.select, e).get)
  }

  test("WHERE") {
    val a = Sparql()
    val e = """
PREFIX empP : <http://hr.example/DB/Employee#>
SELECT ?empName ?manageName
 WHERE {
         ?emp      empP:lastName   ?empName
       }
"""
    val tps =
      Select(
	SparqlAttributeList(List(Var("empName"), Var("manageName"))),
	TriplesBlock(
	  List(
	    TriplePattern(
	      SVar(Var("emp")),
		PUri(Stem("http://hr.example/DB"),Rel("Employee"),Attr("lastName")),
	      OVar(Var("empName"))))))
    assert(tps === a.parseAll(a.select, e).get)
  }

  test("SELECT with FILTER") {
    val a = Sparql()
    val e = """
SELECT ?empName ?manageName {
?emp      <http://hr.example/DB/Employee#lastName>   ?empName
FILTER(?manBday < ?empBday && ?grandManBday < ?manBday)
}
"""
    val tps =
      Select(
	SparqlAttributeList(List(Var("empName"), Var("manageName"))),
	TableFilter(
	  TriplesBlock(
	    List(
	      TriplePattern(
		SVar(Var("emp")),
		PUri(Stem("http://hr.example/DB"),Rel("Employee"),Attr("lastName")),
		OVar(Var("empName"))))),
	  Expression(List(
	    PrimaryExpressionLt(SparqlTermExpression(TermVar(Var("manBday"))),
				      SparqlTermExpression(TermVar(Var("empBday")))), 
	    PrimaryExpressionLt(SparqlTermExpression(TermVar(Var("grandManBday"))),
				      SparqlTermExpression(TermVar(Var("manBday"))))))))
    assert(tps === a.parseAll(a.select, e).get)
  }

  test("SQLbgp") {
    val a = Sparql()
    val e = """
SELECT ?empName ?manageName {
?emp      <http://hr.example/DB/Employee#lastName>   ?empName .
?emp      <http://hr.example/DB/Employee#manager>    ?manager .
?manager  <http://hr.example/DB/Employee#lastName>   ?managName
}
"""
    val tps =
      Select(
	SparqlAttributeList(List(Var("empName"), Var("manageName"))),
	TriplesBlock(
	  List(
	    TriplePattern(
	      SVar(Var("emp")),
		PUri(Stem("http://hr.example/DB"),Rel("Employee"),Attr("lastName")),
	      OVar(Var("empName"))),
	    TriplePattern(
	      SVar(Var("emp")),
	      PUri(Stem("http://hr.example/DB"),Rel("Employee"),Attr("manager")),
	      OVar(Var("manager"))),
	    TriplePattern(
	      SVar(Var("manager")),
	      PUri(Stem("http://hr.example/DB"),Rel("Employee"),Attr("lastName")),
	      OVar(Var("managName"))))))
    assert(tps === a.parseAll(a.select, e).get)
  }

  test("parse filter1") {
    val a = Sparql()
    val e = """
PREFIX empP : <http://hr.example/DB/Employee#>
PREFIX manP : <http://hr.example/DB/Manage#>
SELECT ?empName ?grandManagName {
         ?emp          empP:lastName   ?empName .
         ?emp          empP:birthday   ?empBday .
         ?lower        manP:manages   ?emp .
         ?lower        manP:manager   ?manager .
         ?manager      empP:birthday   ?manBday .
         ?upper        manP:manages   ?manager .
         ?upper        manP:manager   ?grandManager .
         ?grandManager empP:birthday   ?grandManBday .
         ?grandManager empP:lastName   ?grandManagName
         FILTER (?manBday < ?empBday && ?grandManBday < ?manBday)
}
"""
    a.parseAll(a.select, e).get
  }

  test("parse a nested bgp") {
    val a = Sparql()
    val e = """
SELECT ?x { { ?x <http://hr.example/DB/Employee#manager> ?y} }
"""
    val tps =
      Select(
	SparqlAttributeList(List(Var("x"))),
	TriplesBlock(
	  List(
	    TriplePattern(
	      SVar(Var("x")),
	      PUri(Stem("http://hr.example/DB"),Rel("Employee"),Attr("manager")),
	      OVar(Var("y"))))))
    assert(tps === a.parseAll(a.select, e).get)
  }

  test("parse a conjunction") {
    val a = Sparql()
    val e = """
SELECT ?x { { ?x <http://hr.example/DB/Employee#manager> ?y} { ?x <http://hr.example/DB/Employee#manager> ?y} }
"""
    val tps =
      Select(
	SparqlAttributeList(List(Var("x"))),
	TableConjunction(List(
	  TriplesBlock(
	    List(
	      TriplePattern(
		SVar(Var("x")),
		PUri(Stem("http://hr.example/DB"),Rel("Employee"),Attr("manager")),
		OVar(Var("y"))))),
	  TriplesBlock(
	    List(
	      TriplePattern(
		SVar(Var("x")),
		PUri(Stem("http://hr.example/DB"),Rel("Employee"),Attr("manager")),
		OVar(Var("y"))))))))
    assert(tps === a.parseAll(a.select, e).get)
  }

  test("parse a disjunction") {
    val a = Sparql()
    val e = """
SELECT ?x { { ?x <http://hr.example/DB/Employee#manager> ?y} UNION { ?x <http://hr.example/DB/Employee#manager> ?y} }
"""
    val tps =
      Select(
	SparqlAttributeList(List(Var("x"))),
	TableDisjunction(List(
	  TriplesBlock(
	    List(
	      TriplePattern(
		SVar(Var("x")),
		PUri(Stem("http://hr.example/DB"),Rel("Employee"),Attr("manager")),
		OVar(Var("y"))))),
	  TriplesBlock(
	    List(
	      TriplePattern(
		SVar(Var("x")),
		PUri(Stem("http://hr.example/DB"),Rel("Employee"),Attr("manager")),
		OVar(Var("y"))))))))
    assert(tps === a.parseAll(a.select, e).get)
  }

  test("parse an optional") {
    val a = Sparql()
    val e = """
SELECT ?x { { ?x <http://hr.example/DB/Employee#manager> ?y} OPTIONAL { ?x <http://hr.example/DB/Employee#manager> ?y} }
"""
    val tps =
      Select(
	SparqlAttributeList(List(Var("x"))),
	TableConjunction(List(
	  TriplesBlock(
	    List(
	      TriplePattern(
		SVar(Var("x")),
		PUri(Stem("http://hr.example/DB"),Rel("Employee"),Attr("manager")),
		OVar(Var("y"))))),
	  OptionalGraphPattern(
	    TriplesBlock(
	      List(
		TriplePattern(
		  SVar(Var("x")),
		  PUri(Stem("http://hr.example/DB"),Rel("Employee"),Attr("manager")),
		  OVar(Var("y")))))))))
    assert(tps === a.parseAll(a.select, e).get)
  }

  test("parse a leading optional") {
    val a = Sparql()
    val e = """
SELECT ?x { OPTIONAL { ?x <http://hr.example/DB/Employee#manager> ?y} }
"""
    val tps =
      Select(
	SparqlAttributeList(List(Var("x"))),
	OptionalGraphPattern(
	  TriplesBlock(
	    List(
	      TriplePattern(
		SVar(Var("x")),
		PUri(Stem("http://hr.example/DB"),Rel("Employee"),Attr("manager")),
		OVar(Var("y")))))))
    assert(tps === a.parseAll(a.select, e).get)
  }

  test("parse disj1") {
    val a = Sparql()
    val e = """
SELECT ?name
       { ?who <http://hr.example/DB/Employee#lastName> "Smith"^^<http://www.w3.org/2001/XMLSchema#string>
         { ?above   <http://hr.example/DB/Manage#manages> ?who .
           ?above   <http://hr.example/DB/Manage#manager> ?manager .
           ?manager <http://hr.example/DB/Employee#lastName>  ?name }
         UNION
         { ?below   <http://hr.example/DB/Manage#manager> ?who .
           ?below   <http://hr.example/DB/Manage#manages> ?managed .
           ?managed <http://hr.example/DB/Employee#lastName>  ?name } }
"""
    val tps =
      Select(
	SparqlAttributeList(List(Var("name"))),
	TableConjunction(List(
	  TriplesBlock(
	    List(
	      TriplePattern(
		SVar(Var("who")),
		PUri(Stem("http://hr.example/DB"),Rel("Employee"),Attr("lastName")),
		OLit(Literal(RDFLiteral("Smith",Datatype(new URI("http://www.w3.org/2001/XMLSchema#string")))))))),
	  TableDisjunction(List(
	    TriplesBlock(
	      List(
		TriplePattern(
		  SVar(Var("above")),
		  PUri(Stem("http://hr.example/DB"),Rel("Manage"),Attr("manages")),
		  OVar(Var("who"))),
		TriplePattern(
		  SVar(Var("above")),
		  PUri(Stem("http://hr.example/DB"),Rel("Manage"),Attr("manager")),
		  OVar(Var("manager"))),
		TriplePattern(
		  SVar(Var("manager")),
		  PUri(Stem("http://hr.example/DB"),Rel("Employee"),Attr("lastName")),
		  OVar(Var("name")))
	      )),
	    TriplesBlock(
	      List(
		TriplePattern(
		  SVar(Var("below")),
		  PUri(Stem("http://hr.example/DB"),Rel("Manage"),Attr("manager")),
		  OVar(Var("who"))),
		TriplePattern(
		  SVar(Var("below")),
		  PUri(Stem("http://hr.example/DB"),Rel("Manage"),Attr("manages")),
		  OVar(Var("managed"))),
		TriplePattern(
		  SVar(Var("managed")),
		  PUri(Stem("http://hr.example/DB"),Rel("Employee"),Attr("lastName")),
		  OVar(Var("name")))
	      )))))))
    assert(tps === a.parseAll(a.select, e).get)
  }

  test("parse { BGP A BGP FILTER") {
    val a = Sparql()
    val e = """
PREFIX emplP: <http://hr.example/DB/Employee#>

SELECT ?emp1Name ?emp2Name ?emp3Name
 WHERE { ?emp1     emplP:lastName   ?emp1Name
         OPTIONAL { ?emp1     emplP:birthday   ?birthday }
         ?emp2     emplP:lastName   ?emp2Name
         OPTIONAL { ?emp2     emplP:birthday   ?birthday }
         ?emp4     emplP:birthday   ?birthday
         FILTER ( ?emp1Name < ?emp2Name && ?emp2Name < ?emp3Name && ?emp3Name < ?emp4Name) }
"""
    val tps =
      Select(SparqlAttributeList(List(Var("emp1Name"), Var("emp2Name"), Var("emp3Name"))),
       TableFilter(TableConjunction(List(TriplesBlock(List(TriplePattern(SVar(Var("emp1")), PUri(Stem("http://hr.example/DB"),Rel("Employee"),Attr("lastName")), OVar(Var("emp1Name"))))),
					 OptionalGraphPattern(TriplesBlock(List(TriplePattern(SVar(Var("emp1")), PUri(Stem("http://hr.example/DB"),Rel("Employee"),Attr("birthday")), OVar(Var("birthday")))))),
					 TriplesBlock(List(TriplePattern(SVar(Var("emp2")), PUri(Stem("http://hr.example/DB"),Rel("Employee"),Attr("lastName")), OVar(Var("emp2Name"))))),
					 OptionalGraphPattern(TriplesBlock(List(TriplePattern(SVar(Var("emp2")), PUri(Stem("http://hr.example/DB"),Rel("Employee"),Attr("birthday")), OVar(Var("birthday")))))),
					 TriplesBlock(List(TriplePattern(SVar(Var("emp4")),PUri(Stem("http://hr.example/DB"),Rel("Employee"),Attr("birthday")),OVar(Var("birthday"))))))),
		   Expression(List(PrimaryExpressionLt(SparqlTermExpression(TermVar(Var("emp1Name"))),SparqlTermExpression(TermVar(Var("emp2Name")))), PrimaryExpressionLt(SparqlTermExpression(TermVar(Var("emp2Name"))),SparqlTermExpression(TermVar(Var("emp3Name")))), PrimaryExpressionLt(SparqlTermExpression(TermVar(Var("emp3Name"))),SparqlTermExpression(TermVar(Var("emp4Name"))))))))
    assert(tps === a.parseAll(a.select, e).get)
  }

  test("decompose a predicate uri in stem, rel and attr") {
    val uri = "http://hr.example/our/favorite/DB/Employee#lastName"
    val puri:PUri = Sparql.parsePredicateURI(uri)
    assert(puri === PUri(Stem("http://hr.example/our/favorite/DB"),
			 Rel("Employee"),
			 Attr("lastName")))
  }

  test("decompose a object uri in stem, rel and attr") {
    val uri = "http://hr.example/our/favorite/DB/Employee/id.18#record"
    val objuri:ObjUri = Sparql.parseObjectURI(uri)
    assert(objuri === ObjUri(Stem("http://hr.example/our/favorite/DB"),
			 Rel("Employee"),
			 Attr("id"),
			 CellValue("18")))
  }

}
