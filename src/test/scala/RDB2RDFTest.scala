package w3c.sw

import org.scalatest.FunSuite

class TestBank extends FunSuite {

  test("parse arith") {
    val a = Arith()
    val e = "2 * (3 + 7)"
    println(a.parseAll(a.expr, e))
  }

  test("parse sparql") {
    val a = Sparql()
    val e = """
?emp      emplP:lastName   ?empName .
?emp      emplP:manager    ?manager .
?manager  emplP:lastName   ?managName
"""
    println(a.parseAll(a.triplepatterns, e))
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
