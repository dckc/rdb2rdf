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


}
