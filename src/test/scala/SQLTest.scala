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
    val expected = Select(AttributeList(List(NamedAttribute(AliasAttribute(Alias(Name("emp")),
									Attribute(Name("lastName"))),
							    Attribute(Name("empName"))),
					     NamedAttribute(AliasAttribute(Alias(Name("manager")),
									Attribute(Name("lastName"))),
							    Attribute(Name("managName"))))),
			  TableList(List(Join(RelAsAlias(Relation(Name("Employee")),Alias(Name("emp"))),None),
					 Join(RelAsAlias(Relation(Name("Employee")),Alias(Name("manager"))),
					      Some(Expression(List(PrimaryExpressionEq(AliasAttribute(Alias(Name("manager")),
												   Attribute(Name("id"))),
										       RValueAttr(AliasAttribute(Alias(Name("emp")),
													      Attribute(Name("manager"))))))))))),
			  Some(Expression(List(PrimaryExpressionNotNull(AliasAttribute(Alias(Name("emp")),
										    Attribute(Name("lastName")))),
					       PrimaryExpressionNotNull(AliasAttribute(Alias(Name("manager")),
										    Attribute(Name("lastName"))))))))
    assert(expected === (a.parseAll(a.select, e).get))
  }

  test("tup1") {
    val a = Sql()
    val e = """
SELECT emp.lastName AS empName
  FROM Employee AS emp
 WHERE emp.manager=18 AND emp.lastName IS NOT NULL
"""
    val expected = Select(AttributeList(List(NamedAttribute(AliasAttribute(Alias(Name("emp")),
									Attribute(Name("lastName"))),
							    Attribute(Name("empName"))))),
			  TableList(List(Join(RelAsAlias(Relation(Name("Employee")),Alias(Name("emp"))),None))),
			  Some(Expression(List(PrimaryExpressionEq(AliasAttribute(Alias(Name("emp")),
									       Attribute(Name("manager"))),
								   RValueTyped(SQLDatatype.INTEGER,Name("18"))),
					       PrimaryExpressionNotNull(AliasAttribute(Alias(Name("emp")),
										    Attribute(Name("lastName"))))))))
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
    val expected = Select(AttributeList(List(NamedAttribute(AliasAttribute(Alias(Name("emp")),
									Attribute(Name("lastName"))),
							    Attribute(Name("empName"))))),
			  TableList(List(Join(RelAsAlias(Relation(Name("Employee")),Alias(Name("emp"))),None),
					 Join(RelAsAlias(Relation(Name("Employee")),Alias(Name("manager"))),
					      Some(Expression(List(PrimaryExpressionEq(AliasAttribute(Alias(Name("emp")),
												   Attribute(Name("manager"))),
										       RValueAttr(AliasAttribute(Alias(Name("manager")),
													      Attribute(Name("id"))))),
								   PrimaryExpressionEq(AliasAttribute(Alias(Name("manager")),
												   Attribute(Name("lastName"))),
										       RValueTyped(SQLDatatype.STRING,Name("Johnson"))))))))),
			  Some(Expression(List(PrimaryExpressionNotNull(AliasAttribute(Alias(Name("emp")),Attribute(Name("lastName"))))))))
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
    val expected = Select(AttributeList(List(NamedAttribute(AliasAttribute(Alias(Name("emp")), Attribute(Name("lastName"))),
							    Attribute(Name("empName"))),
					     NamedAttribute(AliasAttribute(Alias(Name("grandManager")),Attribute(Name("lastName"))),
							    Attribute(Name("grandManagName"))))),
			  TableList(List(Join(RelAsAlias(Relation(Name("Employee")),Alias(Name("emp"))),None),
					 Join(RelAsAlias(Relation(Name("Manage")),Alias(Name("lower"))),Some(Expression(List(PrimaryExpressionEq(AliasAttribute(Alias(Name("lower")),Attribute(Name("manages"))),RValueAttr(AliasAttribute(Alias(Name("emp")),Attribute(Name("id"))))))))),
					 Join(RelAsAlias(Relation(Name("Employee")),Alias(Name("manager"))),Some(Expression(List(PrimaryExpressionEq(AliasAttribute(Alias(Name("manager")),Attribute(Name("id"))),RValueAttr(AliasAttribute(Alias(Name("lower")),Attribute(Name("manager"))))), PrimaryExpressionLt(AliasAttribute(Alias(Name("manager")),Attribute(Name("birthday"))),RValueAttr(AliasAttribute(Alias(Name("emp")),Attribute(Name("birthday"))))))))),
					 Join(RelAsAlias(Relation(Name("Manage")),Alias(Name("upper"))),Some(Expression(List(PrimaryExpressionEq(AliasAttribute(Alias(Name("upper")),Attribute(Name("manages"))),RValueAttr(AliasAttribute(Alias(Name("manager")),Attribute(Name("id"))))))))),
					 Join(RelAsAlias(Relation(Name("Employee")),Alias(Name("grandManager"))),
					      Some(Expression(List(PrimaryExpressionEq(AliasAttribute(Alias(Name("grandManager")),
												      Attribute(Name("id"))),
										       RValueAttr(AliasAttribute(Alias(Name("upper")),
														 Attribute(Name("manager"))))),
								   PrimaryExpressionLt(AliasAttribute(Alias(Name("grandManager")),
												      Attribute(Name("birthday"))),
										       RValueAttr(AliasAttribute(Alias(Name("manager")),
														 Attribute(Name("birthday"))))))))))),
			  Some(Expression(List(PrimaryExpressionNotNull(AliasAttribute(Alias(Name("emp")),
										       Attribute(Name("lastName")))),
					       PrimaryExpressionNotNull(AliasAttribute(Alias(Name("grandManager")),
										       Attribute(Name("lastName"))))))))
    assert(expected === (a.parseAll(a.select, e).get))
  }


}
