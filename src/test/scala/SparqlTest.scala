package w3c.sw

import org.scalatest.FunSuite
import java.net.URI

class SparqlTest extends FunSuite {

  test("parse a litstring") {
    val a = Sparql()
    val e = """
?emp      <http://hr.example/DB/Employee#lastName>   "bob"^^<http://www.w3.org/2001/XMLSchema#string>
"""
    val expected = TriplePatterns(List(TriplePattern(SVar(Var("emp")),PUri(Stem("http://hr.example/DB"),Rel("Employee"),Attr("lastName")),OLit(Lit("bob",Datatype(new URI("http://www.w3.org/2001/XMLSchema#string")))))))
    assert(expected === (a.parseAll(a.triplepatterns, e).get))
  }

  test("parse a litint") {
    val a = Sparql()
    val e = """
?emp      <http://hr.example/DB/Employee#age>   "21"^^<http://www.w3.org/2001/XMLSchema#integer>
"""
    val expected = TriplePatterns(List(TriplePattern(SVar(Var("emp")),PUri(Stem("http://hr.example/DB"),Rel("Employee"),Attr("age")),OLit(Lit("21",Datatype(new URI("http://www.w3.org/2001/XMLSchema#integer")))))))
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
	    OVar(Var("managName")))))
    assert(tps === a.parseAll(a.triplepatterns, e).get)
  }

  test("decompose a predicate uri in stem, rel and attr") {
    val uri = "http://hr.example/DB/Employee#lastName"
    val puri:PUri = Sparql.parsePredicateURI(uri)
    assert(puri === PUri(Stem("http://hr.example/DB"),
			 Rel("Employee"),
			 Attr("lastName")))
  }

}
