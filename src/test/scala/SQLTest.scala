package w3c.sw

import org.scalatest.FunSuite
import java.net.URI

class SQLTest extends FunSuite {

  test("parse SQLbgp") {
    val a = Sql()
    val e = """
SELECT R_emp.lastName AS A_empName, R_manager.lastName AS A_managName
       FROM Employee AS R_emp
            INNER JOIN Employee AS R_manager
 WHERE R_manager.id=R_emp.manager AND R_emp.lastName IS NOT NULL AND R_manager.lastName IS NOT NULL
"""
    val expected = Select(AttributeList(List(NamedAttribute(RelAliasAttribute(RelAlias(Name("R_emp")),
									      Attribute(Name("lastName"))),
							    AttrAlias(Name("A_empName"))),
					     NamedAttribute(RelAliasAttribute(RelAlias(Name("R_manager")),
									      Attribute(Name("lastName"))),
							    AttrAlias(Name("A_managName"))))),
			  TableList(List(AliasedResource(Relation(Name("Employee")),RelAlias(Name("R_emp"))),
					 AliasedResource(Relation(Name("Employee")),RelAlias(Name("R_manager"))))),
			  Expression(List(
			    PrimaryExpressionEq(RelAliasAttribute(RelAlias(Name("R_manager")),Attribute(Name("id"))),
						RValueAttr(RelAliasAttribute(RelAlias(Name("R_emp")),Attribute(Name("manager"))))),
			    PrimaryExpressionNotNull(RelAliasAttribute(RelAlias(Name("R_emp")),Attribute(Name("lastName")))),
			    PrimaryExpressionNotNull(RelAliasAttribute(RelAlias(Name("R_manager")),Attribute(Name("lastName")))))))
    assert(expected === (a.parseAll(a.select, e).get))
  }

  test("parse tup1") {
    val a = Sql()
    val e = """
SELECT R_emp.lastName AS A_empName
  FROM Employee AS R_emp
 WHERE R_emp.manager=18 AND R_emp.lastName IS NOT NULL
"""
    val expected = Select(AttributeList(List(NamedAttribute(RelAliasAttribute(RelAlias(Name("R_emp")),
									      Attribute(Name("lastName"))),
							    AttrAlias(Name("A_empName"))))),
			  TableList(List(AliasedResource(Relation(Name("Employee")),RelAlias(Name("R_emp"))))),
			  Expression(List(PrimaryExpressionEq(RelAliasAttribute(RelAlias(Name("R_emp")),Attribute(Name("manager"))),
							      RValueTyped(SQLDatatype.INTEGER,Name("18"))),
					  PrimaryExpressionNotNull(RelAliasAttribute(RelAlias(Name("R_emp")),Attribute(Name("lastName")))))))
    assert(expected === (a.parseAll(a.select, e).get))
  }

  test("parse litConst1") {
    val a = Sql()
    val e = """
SELECT R_emp.lastName AS A_empName
  FROM Employee AS R_emp
       INNER JOIN Employee AS R_manager
WHERE R_emp.manager=R_manager.id AND R_manager.lastName="Johnson" AND R_emp.lastName IS NOT NULL
"""
    val expected = Select(AttributeList(List(NamedAttribute(RelAliasAttribute(RelAlias(Name("R_emp")),
									      Attribute(Name("lastName"))),
							    AttrAlias(Name("A_empName"))))),
			  TableList(List(AliasedResource(Relation(Name("Employee")),RelAlias(Name("R_emp"))),
					 AliasedResource(Relation(Name("Employee")),RelAlias(Name("R_manager"))))),
			  Expression(List(PrimaryExpressionEq(RelAliasAttribute(RelAlias(Name("R_emp")),Attribute(Name("manager"))),
							      RValueAttr(RelAliasAttribute(RelAlias(Name("R_manager")),Attribute(Name("id"))))),
					  PrimaryExpressionEq(RelAliasAttribute(RelAlias(Name("R_manager")),Attribute(Name("lastName"))),
							      RValueTyped(SQLDatatype.STRING,Name("Johnson"))),
					  PrimaryExpressionNotNull(RelAliasAttribute(RelAlias(Name("R_emp")),Attribute(Name("lastName")))))))
    assert(expected === (a.parseAll(a.select, e).get))
  }

  test("parse filter1") {
    val a = Sql()
    val e = """
SELECT R_emp.lastName AS A_empName, R_grandManager.lastName AS A_grandManagName
  FROM Employee AS R_emp
       INNER JOIN Manage AS R_lower
       INNER JOIN Employee AS R_manager
       INNER JOIN Manage AS R_upper
       INNER JOIN Employee AS R_grandManager
 WHERE R_lower.manages=R_emp.id AND R_manager.id=R_lower.manager
   AND R_manager.birthday < R_emp.birthday
   AND R_upper.manages=R_manager.id AND R_grandManager.id=R_upper.manager
   AND R_grandManager.birthday < R_manager.birthday
   AND R_emp.lastName IS NOT NULL AND R_grandManager.lastName IS NOT NULL
"""
    val expected = Select(AttributeList(List(NamedAttribute(RelAliasAttribute(RelAlias(Name("R_emp")), Attribute(Name("lastName"))),
							    AttrAlias(Name("A_empName"))),
					     NamedAttribute(RelAliasAttribute(RelAlias(Name("R_grandManager")),Attribute(Name("lastName"))),
							    AttrAlias(Name("A_grandManagName"))))),
			  TableList(List(AliasedResource(Relation(Name("Employee")),RelAlias(Name("R_emp"))),
					 AliasedResource(Relation(Name("Manage")),RelAlias(Name("R_lower"))),
					 AliasedResource(Relation(Name("Employee")),RelAlias(Name("R_manager"))),
					 AliasedResource(Relation(Name("Manage")),RelAlias(Name("R_upper"))),
					 AliasedResource(Relation(Name("Employee")),RelAlias(Name("R_grandManager"))))),
			  Expression(List(PrimaryExpressionEq(RelAliasAttribute(RelAlias(Name("R_lower")),Attribute(Name("manages"))),
							      RValueAttr(RelAliasAttribute(RelAlias(Name("R_emp")),Attribute(Name("id"))))),
					  PrimaryExpressionEq(RelAliasAttribute(RelAlias(Name("R_manager")),Attribute(Name("id"))),
							      RValueAttr(RelAliasAttribute(RelAlias(Name("R_lower")),Attribute(Name("manager"))))),
					  PrimaryExpressionLt(RelAliasAttribute(RelAlias(Name("R_manager")),Attribute(Name("birthday"))),
							      RValueAttr(RelAliasAttribute(RelAlias(Name("R_emp")),Attribute(Name("birthday"))))),
					  PrimaryExpressionEq(RelAliasAttribute(RelAlias(Name("R_upper")),Attribute(Name("manages"))),
							      RValueAttr(RelAliasAttribute(RelAlias(Name("R_manager")),Attribute(Name("id"))))),
					  PrimaryExpressionEq(RelAliasAttribute(RelAlias(Name("R_grandManager")),Attribute(Name("id"))),
							      RValueAttr(RelAliasAttribute(RelAlias(Name("R_upper")),Attribute(Name("manager"))))),
					  PrimaryExpressionLt(RelAliasAttribute(RelAlias(Name("R_grandManager")),Attribute(Name("birthday"))),
							      RValueAttr(RelAliasAttribute(RelAlias(Name("R_manager")),Attribute(Name("birthday"))))),
					  PrimaryExpressionNotNull(RelAliasAttribute(RelAlias(Name("R_emp")),Attribute(Name("lastName")))),
					  PrimaryExpressionNotNull(RelAliasAttribute(RelAlias(Name("R_grandManager")),Attribute(Name("lastName")))))))
    assert(expected === (a.parseAll(a.select, e).get))
  }


}
