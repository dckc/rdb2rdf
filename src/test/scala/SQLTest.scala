package w3c.sw

import org.scalatest.FunSuite
import java.net.URI

class SQLTest extends FunSuite {

  test("parse SQLbgp") {
    val a = Sql()
    val e = """
SELECT R_emp.lastName AS A_empName, R_manager.lastName AS A_managName
       FROM Employee AS R_emp
            INNER JOIN Employee AS R_manager ON R_manager.id=R_emp.manager
 WHERE R_emp.lastName IS NOT NULL AND R_manager.lastName IS NOT NULL
"""
    val expected = Select(AttributeList(List(NamedAttribute(RelAliasAttribute(RelAlias(Name("R_emp")),
									      Attribute(Name("lastName"))),
							    AttrAlias(Name("A_empName"))),
					     NamedAttribute(RelAliasAttribute(RelAlias(Name("R_manager")),
									      Attribute(Name("lastName"))),
							    AttrAlias(Name("A_managName"))))),
			  TableList(List(Join(RelAsRelAlias(Relation(Name("Employee")),RelAlias(Name("R_emp"))),Expression(List())),
					 Join(RelAsRelAlias(Relation(Name("Employee")),RelAlias(Name("R_manager"))),
					      Expression(List(PrimaryExpressionEq(RelAliasAttribute(RelAlias(Name("R_manager")),
												    Attribute(Name("id"))),
										  RValueAttr(RelAliasAttribute(RelAlias(Name("R_emp")),
													       Attribute(Name("manager")))))))))),
			  Expression(List(PrimaryExpressionNotNull(RelAliasAttribute(RelAlias(Name("R_emp")),
										     Attribute(Name("lastName")))),
					  PrimaryExpressionNotNull(RelAliasAttribute(RelAlias(Name("R_manager")),
										     Attribute(Name("lastName")))))))
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
			  TableList(List(Join(RelAsRelAlias(Relation(Name("Employee")),RelAlias(Name("R_emp"))),Expression(List())))),
			  Expression(List(PrimaryExpressionEq(RelAliasAttribute(RelAlias(Name("R_emp")),
										Attribute(Name("manager"))),
							      RValueTyped(SQLDatatype.INTEGER,Name("18"))),
					  PrimaryExpressionNotNull(RelAliasAttribute(RelAlias(Name("R_emp")),
										     Attribute(Name("lastName")))))))
    assert(expected === (a.parseAll(a.select, e).get))
  }

  test("parse litConst1") {
    val a = Sql()
    val e = """
SELECT R_emp.lastName AS A_empName
  FROM Employee AS R_emp
       INNER JOIN Employee AS R_manager ON R_emp.manager=R_manager.id
                                       AND R_manager.lastName="Johnson"
WHERE R_emp.lastName IS NOT NULL
"""
    val expected = Select(AttributeList(List(NamedAttribute(RelAliasAttribute(RelAlias(Name("R_emp")),
									      Attribute(Name("lastName"))),
							    AttrAlias(Name("A_empName"))))),
			  TableList(List(Join(RelAsRelAlias(Relation(Name("Employee")),RelAlias(Name("R_emp"))),Expression(List())),
					 Join(RelAsRelAlias(Relation(Name("Employee")),RelAlias(Name("R_manager"))),
					      Expression(List(PrimaryExpressionEq(RelAliasAttribute(RelAlias(Name("R_emp")),
												    Attribute(Name("manager"))),
										  RValueAttr(RelAliasAttribute(RelAlias(Name("R_manager")),
													       Attribute(Name("id"))))),
							      PrimaryExpressionEq(RelAliasAttribute(RelAlias(Name("R_manager")),
												    Attribute(Name("lastName"))),
										  RValueTyped(SQLDatatype.STRING,Name("Johnson")))))))),
			  Expression(List(PrimaryExpressionNotNull(RelAliasAttribute(RelAlias(Name("R_emp")),Attribute(Name("lastName")))))))
    assert(expected === (a.parseAll(a.select, e).get))
  }

  test("parse filter1") {
    val a = Sql()
    val e = """
SELECT R_emp.lastName AS A_empName, R_grandManager.lastName AS A_grandManagName
  FROM Employee AS R_emp
       INNER JOIN Manage AS R_lower ON R_lower.manages=R_emp.id
       INNER JOIN Employee AS R_manager ON R_manager.id=R_lower.manager
                                         AND R_manager.birthday < R_emp.birthday
       INNER JOIN Manage AS R_upper ON R_upper.manages=R_manager.id
       INNER JOIN Employee AS R_grandManager ON R_grandManager.id=R_upper.manager
                                         AND R_grandManager.birthday < R_manager.birthday
 WHERE R_emp.lastName IS NOT NULL AND R_grandManager.lastName IS NOT NULL
"""
    val expected = Select(AttributeList(List(NamedAttribute(RelAliasAttribute(RelAlias(Name("R_emp")), Attribute(Name("lastName"))),
							    AttrAlias(Name("A_empName"))),
					     NamedAttribute(RelAliasAttribute(RelAlias(Name("R_grandManager")),Attribute(Name("lastName"))),
							    AttrAlias(Name("A_grandManagName"))))),
			  TableList(List(Join(RelAsRelAlias(Relation(Name("Employee")),RelAlias(Name("R_emp"))),Expression(List())),
					 Join(RelAsRelAlias(Relation(Name("Manage")),RelAlias(Name("R_lower"))),Expression(List(PrimaryExpressionEq(RelAliasAttribute(RelAlias(Name("R_lower")),Attribute(Name("manages"))),RValueAttr(RelAliasAttribute(RelAlias(Name("R_emp")),Attribute(Name("id")))))))),
					 Join(RelAsRelAlias(Relation(Name("Employee")),RelAlias(Name("R_manager"))),Expression(List(PrimaryExpressionEq(RelAliasAttribute(RelAlias(Name("R_manager")),Attribute(Name("id"))),RValueAttr(RelAliasAttribute(RelAlias(Name("R_lower")),Attribute(Name("manager"))))), PrimaryExpressionLt(RelAliasAttribute(RelAlias(Name("R_manager")),Attribute(Name("birthday"))),RValueAttr(RelAliasAttribute(RelAlias(Name("R_emp")),Attribute(Name("birthday")))))))),
					 Join(RelAsRelAlias(Relation(Name("Manage")),RelAlias(Name("R_upper"))),Expression(List(PrimaryExpressionEq(RelAliasAttribute(RelAlias(Name("R_upper")),Attribute(Name("manages"))),RValueAttr(RelAliasAttribute(RelAlias(Name("R_manager")),Attribute(Name("id")))))))),
					 Join(RelAsRelAlias(Relation(Name("Employee")),RelAlias(Name("R_grandManager"))),
					      Expression(List(PrimaryExpressionEq(RelAliasAttribute(RelAlias(Name("R_grandManager")),
												    Attribute(Name("id"))),
										  RValueAttr(RelAliasAttribute(RelAlias(Name("R_upper")),
													       Attribute(Name("manager"))))),
							      PrimaryExpressionLt(RelAliasAttribute(RelAlias(Name("R_grandManager")),
												    Attribute(Name("birthday"))),
										  RValueAttr(RelAliasAttribute(RelAlias(Name("R_manager")),
													       Attribute(Name("birthday")))))))))),
			  Expression(List(PrimaryExpressionNotNull(RelAliasAttribute(RelAlias(Name("R_emp")),
										     Attribute(Name("lastName")))),
					  PrimaryExpressionNotNull(RelAliasAttribute(RelAlias(Name("R_grandManager")),
										     Attribute(Name("lastName")))))))
    assert(expected === (a.parseAll(a.select, e).get))
  }


}
