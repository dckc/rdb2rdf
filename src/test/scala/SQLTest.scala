package w3c.sw

import org.scalatest.FunSuite
import java.net.URI

class SQLTest extends FunSuite {

  test("parse SQLbgp") {
    // AliasedResource(Relation(Name("Employee")),RelAlias(Name("R_emp")))
    val a = Sql()
    val e = """
SELECT R_emp.lastName AS A_empName, R_manager.lastName AS A_managName
       FROM Employee AS R_emp
            INNER JOIN Employee AS R_manager
 WHERE R_manager.id=R_emp.manager AND R_emp.lastName IS NOT NULL AND R_manager.lastName IS NOT NULL
"""
    val expected = Select(AttributeList(Set(NamedAttribute(RelAliasAttribute(RelAlias(Name("R_emp")),
									      Attribute(Name("lastName"))),
							    AttrAlias(Name("A_empName"))),
					     NamedAttribute(RelAliasAttribute(RelAlias(Name("R_manager")),
									      Attribute(Name("lastName"))),
							    AttrAlias(Name("A_managName"))))),
			  TableList(Set(AliasedResource(Relation(Name("Employee")),RelAlias(Name("R_emp"))),
					 AliasedResource(Relation(Name("Employee")),RelAlias(Name("R_manager"))))),
			  Expression(Set(
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
    val expected = Select(AttributeList(Set(NamedAttribute(RelAliasAttribute(RelAlias(Name("R_emp")),
									      Attribute(Name("lastName"))),
							    AttrAlias(Name("A_empName"))))),
			  TableList(Set(AliasedResource(Relation(Name("Employee")),RelAlias(Name("R_emp"))))),
			  Expression(Set(PrimaryExpressionEq(RelAliasAttribute(RelAlias(Name("R_emp")),Attribute(Name("manager"))),
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
    val expected = Select(AttributeList(Set(NamedAttribute(RelAliasAttribute(RelAlias(Name("R_emp")),
									      Attribute(Name("lastName"))),
							    AttrAlias(Name("A_empName"))))),
			  TableList(Set(AliasedResource(Relation(Name("Employee")),RelAlias(Name("R_emp"))),
					 AliasedResource(Relation(Name("Employee")),RelAlias(Name("R_manager"))))),
			  Expression(Set(PrimaryExpressionEq(RelAliasAttribute(RelAlias(Name("R_emp")),Attribute(Name("manager"))),
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
    val expected = Select(AttributeList(Set(NamedAttribute(RelAliasAttribute(RelAlias(Name("R_emp")), Attribute(Name("lastName"))),
							   AttrAlias(Name("A_empName"))),
					    NamedAttribute(RelAliasAttribute(RelAlias(Name("R_grandManager")),Attribute(Name("lastName"))),
							   AttrAlias(Name("A_grandManagName"))))),
			  TableList(Set(AliasedResource(Relation(Name("Employee")),RelAlias(Name("R_emp"))),
					AliasedResource(Relation(Name("Manage")),RelAlias(Name("R_lower"))),
					AliasedResource(Relation(Name("Employee")),RelAlias(Name("R_manager"))),
					AliasedResource(Relation(Name("Manage")),RelAlias(Name("R_upper"))),
					AliasedResource(Relation(Name("Employee")),RelAlias(Name("R_grandManager"))))),
			  Expression(Set(PrimaryExpressionEq(RelAliasAttribute(RelAlias(Name("R_lower")),Attribute(Name("manages"))),
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

  test("parse disj1") {
    val a = Sql()
    val e = """
SELECT R_union1.name AS A_name
  FROM Employee AS R_who
       INNER JOIN (
         SELECT R_manager.lastName AS A_name, R_above.manages AS A_who
                FROM Manage AS R_above
                INNER JOIN Employee AS R_manager
          WHERE R_above.manager=R_manager.id AND R_manager.lastName IS NOT NULL
       UNION
         SELECT R_managed.lastName AS A_name, R_below.manager AS A_who
                FROM Manage AS R_below
                INNER JOIN Employee AS R_managed
          WHERE R_below.manages=R_managed.id AND R_managed.lastName IS NOT NULL
       ) AS R_union1
 WHERE R_union1.A_who=R_who.id AND R_who.lastName="Smith"
"""
    val expected = Select(AttributeList(Set(NamedAttribute(RelAliasAttribute(RelAlias(Name("R_union1")), Attribute(Name("name"))),
							   AttrAlias(Name("A_name"))))),
			  TableList(Set(AliasedResource(Relation(Name("Employee")),RelAlias(Name("R_who"))),
					AliasedResource(Subselect(Union(Set(
					  Select(AttributeList(Set(NamedAttribute(RelAliasAttribute(RelAlias(Name("R_manager")), Attribute(Name("lastName"))),
										  AttrAlias(Name("A_name"))), 
								   NamedAttribute(RelAliasAttribute(RelAlias(Name("R_above")), Attribute(Name("manages"))),
										  AttrAlias(Name("A_who"))))),
						 TableList(Set(
						   AliasedResource(Relation(Name("Manage")),RelAlias(Name("R_above"))),
						   AliasedResource(Relation(Name("Employee")),RelAlias(Name("R_manager")))
						 )), 
						 Expression(Set(PrimaryExpressionEq(RelAliasAttribute(RelAlias(Name("R_above")),Attribute(Name("manager"))),
									 RValueAttr(RelAliasAttribute(RelAlias(Name("R_manager")),Attribute(Name("id"))))),
						     PrimaryExpressionNotNull(RelAliasAttribute(RelAlias(Name("R_manager")),Attribute(Name("lastName"))))))), 
					  Select(AttributeList(Set(NamedAttribute(RelAliasAttribute(RelAlias(Name("R_managed")), Attribute(Name("lastName"))),
										  AttrAlias(Name("A_name"))), 
								   NamedAttribute(RelAliasAttribute(RelAlias(Name("R_below")), Attribute(Name("manager"))),
										  AttrAlias(Name("A_who"))))),
						 TableList(Set(
						   AliasedResource(Relation(Name("Manage")),RelAlias(Name("R_below"))),
						   AliasedResource(Relation(Name("Employee")),RelAlias(Name("R_managed")))
						 )), 
						 Expression(Set(PrimaryExpressionEq(RelAliasAttribute(RelAlias(Name("R_below")),Attribute(Name("manages"))),
									 RValueAttr(RelAliasAttribute(RelAlias(Name("R_managed")),Attribute(Name("id"))))),
						     PrimaryExpressionNotNull(RelAliasAttribute(RelAlias(Name("R_managed")),Attribute(Name("lastName")))))))))),
							RelAlias(Name("R_union1"))))), 
			  Expression(Set(PrimaryExpressionEq(RelAliasAttribute(RelAlias(Name("R_union1")),Attribute(Name("A_who"))),
							     RValueAttr(RelAliasAttribute(RelAlias(Name("R_who")),Attribute(Name("id"))))),
					 PrimaryExpressionEq(RelAliasAttribute(RelAlias(Name("R_who")),Attribute(Name("lastName"))),
							     RValueTyped(SQLDatatype.STRING,Name("Smith"))))))
    assert(expected === (a.parseAll(a.select, e).get))
  }

}
