package w3c.sw

import org.scalatest.FunSuite
import java.net.URI

class TestBank extends FunSuite {

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

  test("SQLbgp") {
    val a = Sql()
    val e = """
SELECT emp.lastName AS empName, manager.lastName AS managName
       FROM Employee AS emp
            INNER JOIN Employee AS manager ON manager.id=emp.manager
 WHERE emp.lastName IS NOT NULL AND manager.lastName IS NOT NULL
"""
    println(a.parseAll(a.select, e))
  }

  test("tup1") {
    val a = Sql()
    val e = """
SELECT emp.lastName AS empName
  FROM Employee AS emp
 WHERE emp.manager=18 AND emp.lastName IS NOT NULL
"""
    println(a.parseAll(a.select, e))
  }

  test("litConst1") {
    val a = Sql()
    val e = """
SELECT emp.lastName AS empName
  FROM Employee AS emp
       INNER JOIN Employee AS manager ON emp.manager=manager.id
                                       AND manager.lastName="Johnson"
WHERE emp.lastName IS NOT NULL
"""
    println(a.parseAll(a.select, e))
  }

  test("filter1") {
    val a = Sql()
    val e = """
SELECT emp.lastName AS empName, grandManager.lastName AS grandManagName
  FROM Employee AS emp
       INNER JOIN Manage AS lower ON lower.manages=emp.id
       INNER JOIN Employee AS manager ON manager.id=lower.manager
                                         AND manager.birthday < emp.birthday
       INNER JOIN Manage AS upper ON upper.manages=manager.id
       INNER JOIN Employee AS grandManager ON grandManager.id=upper.manager
                                         AND grandManager.birthday < manager.birthday
 WHERE emp.lastName IS NOT NULL AND grandManager.lastName IS NOT NULL
"""
    println(a.parseAll(a.select, e))
  }


}
