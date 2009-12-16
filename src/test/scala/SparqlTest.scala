package w3c.sw

import org.scalatest.FunSuite
import java.net.URI

class SparqlTest extends FunSuite {

  test("parse a litstring") {
    val a = Sparql()
    val e = """
?emp      <http://hr.example/DB/Employee#lastName>   "bob"^^<http://www.w3.org/2001/XMLSchema#string>
"""
    val expected = TriplePatterns(List(TriplePattern(SVar(Var("emp")),PUri(Stem("http://hr.example/DB"),Rel("Employee"),Attr("lastName")),OLit(SparqlLiteral(RDFLiteral("bob",Datatype(new URI("http://www.w3.org/2001/XMLSchema#string"))))))), SparqlExpression(List()))
    assert(expected === (a.parseAll(a.triplepatterns, e).get))
  }

  test("parse a litint") {
    val a = Sparql()
    val e = """
?emp      <http://hr.example/DB/Employee#age>   "21"^^<http://www.w3.org/2001/XMLSchema#integer>
"""
    val expected = TriplePatterns(List(TriplePattern(SVar(Var("emp")),PUri(Stem("http://hr.example/DB"),Rel("Employee"),Attr("age")),OLit(SparqlLiteral(RDFLiteral("21",Datatype(new URI("http://www.w3.org/2001/XMLSchema#integer"))))))), SparqlExpression(List()))
    assert(expected === (a.parseAll(a.triplepatterns, e).get))
  }

  test("parse a triplepatterns") {
    val a = Sparql()
    val e = """
?emp      <http://hr.example/DB/Employee#lastName>   ?empName .
?emp      <http://hr.example/DB/Employee#manager>    ?manager .
?manager  <http://hr.example/DB/Employee#lastName>   ?managName
"""
    val tps =
      TriplePatterns(
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
	    OVar(Var("managName")))), SparqlExpression(List()))
    assert(tps === a.parseAll(a.triplepatterns, e).get)
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

  test("SparqlPrimaryExpressionEq") {
    val a = Sparql()
    val e = """
?emp<?emp
"""
    val expected = SparqlPrimaryExpressionLt(SparqlTermExpression(TermVar(Var("emp"))), SparqlTermExpression(TermVar(Var("emp"))))
    assert(expected === (a.parseAll(a.primaryexpression, e).get))
  }

  test("SparqlExpression") {
    val a = Sparql()
    val e = """
?manBday < ?empBday && ?grandManBday < ?manBday
"""
    val expected = SparqlExpression(List(
      SparqlPrimaryExpressionLt(SparqlTermExpression(TermVar(Var("manBday"))), SparqlTermExpression(TermVar(Var("empBday")))), 
      SparqlPrimaryExpressionLt(SparqlTermExpression(TermVar(Var("grandManBday"))), SparqlTermExpression(TermVar(Var("manBday"))))))
    assert(expected === (a.parseAll(a.expression, e).get))
  }

  test("FILTER") {
    val a = Sparql()
    val e = """
FILTER(?manBday < ?empBday && ?grandManBday < ?manBday)
"""
    val expected = SparqlExpression(List(
      SparqlPrimaryExpressionLt(SparqlTermExpression(TermVar(Var("manBday"))), SparqlTermExpression(TermVar(Var("empBday")))), 
      SparqlPrimaryExpressionLt(SparqlTermExpression(TermVar(Var("grandManBday"))), SparqlTermExpression(TermVar(Var("manBday"))))))
    assert(expected === (a.parseAll(a.filter, e).get))
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
      SparqlSelect(
	SparqlAttributeList(List(Var("empName"), Var("manageName"))),
	TriplePatterns(
	  List(
	    TriplePattern(
	      SVar(Var("emp")),
		PUri(Stem("http://hr.example/DB"),Rel("Employee"),Attr("lastName")),
	      OVar(Var("empName")))), 
	  SparqlExpression(List(
	    SparqlPrimaryExpressionLt(SparqlTermExpression(TermVar(Var("manBday"))),
				      SparqlTermExpression(TermVar(Var("empBday")))), 
	    SparqlPrimaryExpressionLt(SparqlTermExpression(TermVar(Var("grandManBday"))),
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
      SparqlSelect(
	SparqlAttributeList(List(Var("empName"), Var("manageName"))),
	TriplePatterns(
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
	      OVar(Var("managName")))), SparqlExpression(List())))
    assert(tps === a.parseAll(a.select, e).get)
  }

  test("parse filter1") {
    val a = Sparql()
    val e = """
SELECT ?empName ?grandManagName {
         ?emp          <http://hr.example/DB/Employee#lastName>   ?empName .
         ?emp          <http://hr.example/DB/Employee#birthday>   ?empBday .
         ?lower        <http://hr.example/DB/Manage#manages>   ?emp .
         ?lower        <http://hr.example/DB/Manage#manager>   ?manager .
         ?manager      <http://hr.example/DB/Employee#birthday>   ?manBday .
         ?upper        <http://hr.example/DB/Manage#manages>   ?manager .
         ?upper        <http://hr.example/DB/Manage#manager>   ?grandManager .
         ?grandManager <http://hr.example/DB/Employee#birthday>   ?grandManBday .
         ?grandManager <http://hr.example/DB/Employee#lastName>   ?grandManagName
         FILTER (?manBday < ?empBday && ?grandManBday < ?manBday)
}
"""
    a.parseAll(a.select, e).get
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
