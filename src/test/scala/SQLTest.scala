package w3c.sw

import org.scalatest.FunSuite
import java.net.URI

class SQLTest extends FunSuite {

  test("SQLbgp") {
    val a = Sql()
    val e = """
SELECT emp.lastName AS empName, manager.lastName AS managName
       FROM Employee AS emp
            INNER JOIN Employee AS manager ON manager.id=emp.manager
 WHERE emp.lastName IS NOT NULL AND manager.lastName IS NOT NULL
"""
    val expected = Select(AttributeList(List(NamedAttribute(FQAttribute(Relation(Name("emp")),Attribute(Name("lastName"))),Attribute(Name("empName"))), NamedAttribute(FQAttribute(Relation(Name("manager")),Attribute(Name("lastName"))),Attribute(Name("managName"))))),TableList(List(Join(RelAsAlias(Relation(Name("Employee")),Relation(Name("emp"))),None),Join(RelAsAlias(Relation(Name("Employee")),Relation(Name("manager"))),Some(Expression(List(PrimaryExpressionEq(FQAttribute(Relation(Name("manager")),Attribute(Name("id"))),RValueAttr(FQAttribute(Relation(Name("emp")),Attribute(Name("manager"))))))))))),Some(Expression(List(PrimaryExpressionNotNull(FQAttribute(Relation(Name("emp")),Attribute(Name("lastName")))), PrimaryExpressionNotNull(FQAttribute(Relation(Name("manager")),Attribute(Name("lastName"))))))))
    assert(expected === (a.parseAll(a.select, e).get))
  }

  test("tup1") {
    val a = Sql()
    val e = """
SELECT emp.lastName AS empName
  FROM Employee AS emp
 WHERE emp.manager=18 AND emp.lastName IS NOT NULL
"""
    val expected = Select(AttributeList(List(NamedAttribute(FQAttribute(Relation(Name("emp")),Attribute(Name("lastName"))),Attribute(Name("empName"))))),TableList(List(Join(RelAsAlias(Relation(Name("Employee")),Relation(Name("emp"))),None))),Some(Expression(List(PrimaryExpressionEq(FQAttribute(Relation(Name("emp")),Attribute(Name("manager"))),RValueTyped(SQLDatatype.INTEGER,Name("18"))), PrimaryExpressionNotNull(FQAttribute(Relation(Name("emp")),Attribute(Name("lastName"))))))))
    assert(expected === (a.parseAll(a.select, e).get))
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
    val expected = Select(AttributeList(List(NamedAttribute(FQAttribute(Relation(Name("emp")),Attribute(Name("lastName"))),Attribute(Name("empName"))))),TableList(List(Join(RelAsAlias(Relation(Name("Employee")),Relation(Name("emp"))),None),Join(RelAsAlias(Relation(Name("Employee")),Relation(Name("manager"))),Some(Expression(List(PrimaryExpressionEq(FQAttribute(Relation(Name("emp")),Attribute(Name("manager"))),RValueAttr(FQAttribute(Relation(Name("manager")),Attribute(Name("id"))))), PrimaryExpressionEq(FQAttribute(Relation(Name("manager")),Attribute(Name("lastName"))),RValueTyped(SQLDatatype.STRING,Name("Johnson"))))))))),Some(Expression(List(PrimaryExpressionNotNull(FQAttribute(Relation(Name("emp")),Attribute(Name("lastName"))))))))
    assert(expected === (a.parseAll(a.select, e).get))
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
    val expected = Select(AttributeList(List(NamedAttribute(FQAttribute(Relation(Name("emp")),Attribute(Name("lastName"))),Attribute(Name("empName"))), NamedAttribute(FQAttribute(Relation(Name("grandManager")),Attribute(Name("lastName"))),Attribute(Name("grandManagName"))))),TableList(List(Join(RelAsAlias(Relation(Name("Employee")),Relation(Name("emp"))),None),Join(RelAsAlias(Relation(Name("Manage")),Relation(Name("lower"))),Some(Expression(List(PrimaryExpressionEq(FQAttribute(Relation(Name("lower")),Attribute(Name("manages"))),RValueAttr(FQAttribute(Relation(Name("emp")),Attribute(Name("id"))))))))), Join(RelAsAlias(Relation(Name("Employee")),Relation(Name("manager"))),Some(Expression(List(PrimaryExpressionEq(FQAttribute(Relation(Name("manager")),Attribute(Name("id"))),RValueAttr(FQAttribute(Relation(Name("lower")),Attribute(Name("manager"))))), PrimaryExpressionLt(FQAttribute(Relation(Name("manager")),Attribute(Name("birthday"))),RValueAttr(FQAttribute(Relation(Name("emp")),Attribute(Name("birthday"))))))))), Join(RelAsAlias(Relation(Name("Manage")),Relation(Name("upper"))),Some(Expression(List(PrimaryExpressionEq(FQAttribute(Relation(Name("upper")),Attribute(Name("manages"))),RValueAttr(FQAttribute(Relation(Name("manager")),Attribute(Name("id"))))))))), Join(RelAsAlias(Relation(Name("Employee")),Relation(Name("grandManager"))),Some(Expression(List(PrimaryExpressionEq(FQAttribute(Relation(Name("grandManager")),Attribute(Name("id"))),RValueAttr(FQAttribute(Relation(Name("upper")),Attribute(Name("manager"))))), PrimaryExpressionLt(FQAttribute(Relation(Name("grandManager")),Attribute(Name("birthday"))),RValueAttr(FQAttribute(Relation(Name("manager")),Attribute(Name("birthday"))))))))))),Some(Expression(List(PrimaryExpressionNotNull(FQAttribute(Relation(Name("emp")),Attribute(Name("lastName")))), PrimaryExpressionNotNull(FQAttribute(Relation(Name("grandManager")),Attribute(Name("lastName"))))))))
    assert(expected === (a.parseAll(a.select, e).get))
  }


}
