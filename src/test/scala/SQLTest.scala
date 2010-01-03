package w3c.sw

import org.scalatest.FunSuite
import java.net.URI

class SQLTest extends FunSuite {

  test("parse ANDexpression") {
    // AliasedResource(Relation(Name("Employee")),RelAlias(Name("R_emp")))
    val a = Sql()
    val e = """
R_manager.id=R_emp.manager AND R_emp.lastName IS NOT NULL AND R_manager.lastName IS NOT NULL
"""
    val expected = ExprConjunction(Set(
      RelationalExpressionEq(PrimaryExpressionAttr(RelAliasAttribute(RelAlias(Name("R_manager")),Attribute(Name("id")))),
			     PrimaryExpressionAttr(RelAliasAttribute(RelAlias(Name("R_emp")),Attribute(Name("manager"))))),
      RelationalExpressionNotNull(PrimaryExpressionAttr(RelAliasAttribute(RelAlias(Name("R_emp")),Attribute(Name("lastName"))))),
      RelationalExpressionNotNull(PrimaryExpressionAttr(RelAliasAttribute(RelAlias(Name("R_manager")),Attribute(Name("lastName")))))))
    assert(expected === (a.parseAll(a.expression, e).get))
  }

  test("parse ORexpression") {
    // AliasedResource(Relation(Name("Employee")),RelAlias(Name("R_emp")))
    val a = Sql()
    val e = """
R_manager.id=R_emp.manager OR R_emp.lastName IS NOT NULL OR R_manager.lastName IS NOT NULL
"""
    val expected = ExprDisjunction(Set(
      RelationalExpressionEq(PrimaryExpressionAttr(RelAliasAttribute(RelAlias(Name("R_manager")),Attribute(Name("id")))),
			     PrimaryExpressionAttr(RelAliasAttribute(RelAlias(Name("R_emp")),Attribute(Name("manager"))))),
      RelationalExpressionNotNull(PrimaryExpressionAttr(RelAliasAttribute(RelAlias(Name("R_emp")),Attribute(Name("lastName"))))),
      RelationalExpressionNotNull(PrimaryExpressionAttr(RelAliasAttribute(RelAlias(Name("R_manager")),Attribute(Name("lastName")))))))
    assert(expected === (a.parseAll(a.expression, e).get))
  }

  test("parse nested expression") {
    // AliasedResource(Relation(Name("Employee")),RelAlias(Name("R_emp")))
    val a = Sql()
    val e = """
( R_manager.id=R_emp.manager OR R_emp.lastName IS NOT NULL OR R_manager.lastName IS NOT NULL )
"""
    val expected = ExprDisjunction(Set(
      RelationalExpressionEq(PrimaryExpressionAttr(RelAliasAttribute(RelAlias(Name("R_manager")),Attribute(Name("id")))),
			     PrimaryExpressionAttr(RelAliasAttribute(RelAlias(Name("R_emp")),Attribute(Name("manager"))))),
      RelationalExpressionNotNull(PrimaryExpressionAttr(RelAliasAttribute(RelAlias(Name("R_emp")),Attribute(Name("lastName"))))),
      RelationalExpressionNotNull(PrimaryExpressionAttr(RelAliasAttribute(RelAlias(Name("R_manager")),Attribute(Name("lastName")))))))
    assert(expected === (a.parseAll(a.expression, e).get))
  }

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
			  TableList(Set(InnerJoin(AliasedResource(Relation(Name("Employee")),RelAlias(Name("R_emp")))),
					InnerJoin(AliasedResource(Relation(Name("Employee")),RelAlias(Name("R_manager")))))),
			  Some(ExprConjunction(Set(
			    RelationalExpressionEq(PrimaryExpressionAttr(RelAliasAttribute(RelAlias(Name("R_manager")),Attribute(Name("id")))),
						   PrimaryExpressionAttr(RelAliasAttribute(RelAlias(Name("R_emp")),Attribute(Name("manager"))))),
			    RelationalExpressionNotNull(PrimaryExpressionAttr(RelAliasAttribute(RelAlias(Name("R_emp")),Attribute(Name("lastName"))))),
			    RelationalExpressionNotNull(PrimaryExpressionAttr(RelAliasAttribute(RelAlias(Name("R_manager")),Attribute(Name("lastName")))))))))
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
			  TableList(Set(InnerJoin(AliasedResource(Relation(Name("Employee")),RelAlias(Name("R_emp")))))),
			  Some(ExprConjunction(Set(RelationalExpressionEq(PrimaryExpressionAttr(RelAliasAttribute(RelAlias(Name("R_emp")),Attribute(Name("manager")))),
									  PrimaryExpressionTyped(SQLDatatype.INTEGER,Name("18"))),
					  RelationalExpressionNotNull(PrimaryExpressionAttr(RelAliasAttribute(RelAlias(Name("R_emp")),Attribute(Name("lastName")))))))))
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
			  TableList(Set(InnerJoin(AliasedResource(Relation(Name("Employee")),RelAlias(Name("R_emp")))),
					InnerJoin(AliasedResource(Relation(Name("Employee")),RelAlias(Name("R_manager")))))),
			  Some(ExprConjunction(Set(RelationalExpressionEq(PrimaryExpressionAttr(RelAliasAttribute(RelAlias(Name("R_emp")),Attribute(Name("manager")))),
									  PrimaryExpressionAttr(RelAliasAttribute(RelAlias(Name("R_manager")),Attribute(Name("id"))))),
					  RelationalExpressionEq(PrimaryExpressionAttr(RelAliasAttribute(RelAlias(Name("R_manager")),Attribute(Name("lastName")))),
								 PrimaryExpressionTyped(SQLDatatype.STRING,Name("Johnson"))),
					  RelationalExpressionNotNull(PrimaryExpressionAttr(RelAliasAttribute(RelAlias(Name("R_emp")),Attribute(Name("lastName")))))))))
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
			  TableList(Set(InnerJoin(AliasedResource(Relation(Name("Employee")),RelAlias(Name("R_emp")))),
					InnerJoin(AliasedResource(Relation(Name("Manage")),RelAlias(Name("R_lower")))),
					InnerJoin(AliasedResource(Relation(Name("Employee")),RelAlias(Name("R_manager")))),
					InnerJoin(AliasedResource(Relation(Name("Manage")),RelAlias(Name("R_upper")))),
					InnerJoin(AliasedResource(Relation(Name("Employee")),RelAlias(Name("R_grandManager")))))),
			  Some(ExprConjunction(Set(RelationalExpressionEq(PrimaryExpressionAttr(RelAliasAttribute(RelAlias(Name("R_lower")),Attribute(Name("manages")))),
									  PrimaryExpressionAttr(RelAliasAttribute(RelAlias(Name("R_emp")),Attribute(Name("id"))))),
					 RelationalExpressionEq(PrimaryExpressionAttr(RelAliasAttribute(RelAlias(Name("R_manager")),Attribute(Name("id")))),
								PrimaryExpressionAttr(RelAliasAttribute(RelAlias(Name("R_lower")),Attribute(Name("manager"))))),
					 RelationalExpressionLt(PrimaryExpressionAttr(RelAliasAttribute(RelAlias(Name("R_manager")),Attribute(Name("birthday")))),
								PrimaryExpressionAttr(RelAliasAttribute(RelAlias(Name("R_emp")),Attribute(Name("birthday"))))),
					 RelationalExpressionEq(PrimaryExpressionAttr(RelAliasAttribute(RelAlias(Name("R_upper")),Attribute(Name("manages")))),
								PrimaryExpressionAttr(RelAliasAttribute(RelAlias(Name("R_manager")),Attribute(Name("id"))))),
					 RelationalExpressionEq(PrimaryExpressionAttr(RelAliasAttribute(RelAlias(Name("R_grandManager")),Attribute(Name("id")))),
								PrimaryExpressionAttr(RelAliasAttribute(RelAlias(Name("R_upper")),Attribute(Name("manager"))))),
					 RelationalExpressionLt(PrimaryExpressionAttr(RelAliasAttribute(RelAlias(Name("R_grandManager")),Attribute(Name("birthday")))),
								PrimaryExpressionAttr(RelAliasAttribute(RelAlias(Name("R_manager")),Attribute(Name("birthday"))))),
					 RelationalExpressionNotNull(PrimaryExpressionAttr(RelAliasAttribute(RelAlias(Name("R_emp")),Attribute(Name("lastName"))))),
					 RelationalExpressionNotNull(PrimaryExpressionAttr(RelAliasAttribute(RelAlias(Name("R_grandManager")),Attribute(Name("lastName")))))))))
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
			  TableList(Set(InnerJoin(AliasedResource(Relation(Name("Employee")),RelAlias(Name("R_who")))),
					InnerJoin(AliasedResource(Subselect(Union(Set(
					  Select(AttributeList(Set(NamedAttribute(RelAliasAttribute(RelAlias(Name("R_manager")), Attribute(Name("lastName"))),
										  AttrAlias(Name("A_name"))), 
								   NamedAttribute(RelAliasAttribute(RelAlias(Name("R_above")), Attribute(Name("manages"))),
										  AttrAlias(Name("A_who"))))),
						 TableList(Set(
						   InnerJoin(AliasedResource(Relation(Name("Manage")),RelAlias(Name("R_above")))),
						   InnerJoin(AliasedResource(Relation(Name("Employee")),RelAlias(Name("R_manager"))))
						 )), 
						 Some(ExprConjunction(Set(RelationalExpressionEq(PrimaryExpressionAttr(RelAliasAttribute(RelAlias(Name("R_above")),Attribute(Name("manager")))),
												 PrimaryExpressionAttr(RelAliasAttribute(RelAlias(Name("R_manager")),Attribute(Name("id"))))),
								RelationalExpressionNotNull(PrimaryExpressionAttr(RelAliasAttribute(RelAlias(Name("R_manager")),Attribute(Name("lastName"))))))))), 
					  Select(AttributeList(Set(NamedAttribute(RelAliasAttribute(RelAlias(Name("R_managed")), Attribute(Name("lastName"))),
										  AttrAlias(Name("A_name"))), 
								   NamedAttribute(RelAliasAttribute(RelAlias(Name("R_below")), Attribute(Name("manager"))),
										  AttrAlias(Name("A_who"))))),
						 TableList(Set(
						   InnerJoin(AliasedResource(Relation(Name("Manage")),RelAlias(Name("R_below")))),
						   InnerJoin(AliasedResource(Relation(Name("Employee")),RelAlias(Name("R_managed"))))
						 )), 
						 Some(ExprConjunction(Set(RelationalExpressionEq(PrimaryExpressionAttr(RelAliasAttribute(RelAlias(Name("R_below")),Attribute(Name("manages")))),
												 PrimaryExpressionAttr(RelAliasAttribute(RelAlias(Name("R_managed")),Attribute(Name("id"))))),
								RelationalExpressionNotNull(PrimaryExpressionAttr(RelAliasAttribute(RelAlias(Name("R_managed")),Attribute(Name("lastName")))))))))))),
							RelAlias(Name("R_union1")))))), 
			  Some(ExprConjunction(Set(RelationalExpressionEq(PrimaryExpressionAttr(RelAliasAttribute(RelAlias(Name("R_union1")),Attribute(Name("A_who")))),
									  PrimaryExpressionAttr(RelAliasAttribute(RelAlias(Name("R_who")),Attribute(Name("id"))))),
					 RelationalExpressionEq(PrimaryExpressionAttr(RelAliasAttribute(RelAlias(Name("R_who")),Attribute(Name("lastName")))),
								PrimaryExpressionTyped(SQLDatatype.STRING,Name("Smith")))))))
    assert(expected === (a.parseAll(a.select, e).get))
  }

  test("parse NULL as A_foo") {
    val a = Sql()
    val e = """
SELECT R_above.manages AS A_who, NULL AS A_bday
                FROM Manage AS R_above
          WHERE R_above.id IS NOT NULL
"""
    val expected = Select(AttributeList(Set(NamedAttribute(RelAliasAttribute(RelAlias(Name("R_above")),
									      Attribute(Name("manages"))),
							    AttrAlias(Name("A_who"))),
					     NamedAttribute(ConstNULL(),
							    AttrAlias(Name("A_bday"))))),
			  TableList(Set(InnerJoin(AliasedResource(Relation(Name("Manage")),RelAlias(Name("R_above")))))),
			  Some(
			    RelationalExpressionNotNull(PrimaryExpressionAttr(RelAliasAttribute(RelAlias(Name("R_above")),Attribute(Name("id")))))))
    assert(expected === (a.parseAll(a.select, e).get))
  }

  test("parse CONCAT") {
    val a = Sql()
    val QuotedBaseURI = "\"http://hr.example/DB/\""
    val e = """
SELECT CONCAT(""" + QuotedBaseURI + """, "Employee", "/", "id", ".", R_emp.id, "#record") AS A_emp
       FROM Employee AS R_emp
"""
    val expected = Select(AttributeList(Set(NamedAttribute(Concat(List(PrimaryExpressionTyped(SQLDatatype("String"),Name("http://hr.example/DB/")),
								       PrimaryExpressionTyped(SQLDatatype("String"),Name("Employee")),
								       PrimaryExpressionTyped(SQLDatatype("String"),Name("/")),
								       PrimaryExpressionTyped(SQLDatatype("String"),Name("id")),
								       PrimaryExpressionTyped(SQLDatatype("String"),Name(".")),
								       PrimaryExpressionAttr(RelAliasAttribute(RelAlias(Name("R_emp")),Attribute(Name("id")))),
								       PrimaryExpressionTyped(SQLDatatype("String"),Name("#record")))),
							    AttrAlias(Name("A_emp"))))),
			  TableList(Set(InnerJoin(AliasedResource(Relation(Name("Employee")),RelAlias(Name("R_emp")))))),
			  None)
    assert(expected === (a.parseAll(a.select, e).get))
  }

  test("parse expr disjunction") {
    val a = Sql()
    val e = """
SELECT R_above.manages AS A_who, NULL AS A_bday
                FROM Manage AS R_above
          WHERE (R_above.id IS NOT NULL) OR (R_above.id < 5 AND R_above.id < 3)
"""
    val expected = Select(AttributeList(Set(NamedAttribute(RelAliasAttribute(RelAlias(Name("R_above")),
									      Attribute(Name("manages"))),
							    AttrAlias(Name("A_who"))),
					     NamedAttribute(ConstNULL(),
							    AttrAlias(Name("A_bday"))))),
			  TableList(Set(InnerJoin(AliasedResource(Relation(Name("Manage")),RelAlias(Name("R_above")))))),
			  Some(
			    ExprDisjunction(Set(
			      RelationalExpressionNotNull(PrimaryExpressionAttr(RelAliasAttribute(RelAlias(Name("R_above")),Attribute(Name("id"))))),
			      ExprConjunction(Set(
				RelationalExpressionLt(PrimaryExpressionAttr(RelAliasAttribute(RelAlias(Name("R_above")),Attribute(Name("id")))),
						       PrimaryExpressionTyped(SQLDatatype.INTEGER,Name("5"))),
				RelationalExpressionLt(PrimaryExpressionAttr(RelAliasAttribute(RelAlias(Name("R_above")),Attribute(Name("id")))),
						       PrimaryExpressionTyped(SQLDatatype.INTEGER,Name("3")))
			      ))))))
    assert(expected === (a.parseAll(a.select, e).get))
  }

  test("parse LEFT OUTER JOIN") {
    val a = Sql()
    val e = """
SELECT R_emp.lastName AS A_empName, R_mang.manageName AS A_manageName
       FROM Employee AS R_emp
            LEFT OUTER JOIN Manage AS R_mang ON R_mang.emp=R_emp.id
 WHERE R_emp.lastName IS NOT NULL
"""
    val expected = Select(AttributeList(Set(NamedAttribute(RelAliasAttribute(RelAlias(Name("R_emp")),
									     Attribute(Name("lastName"))),
							    AttrAlias(Name("A_empName"))),
					     NamedAttribute(RelAliasAttribute(RelAlias(Name("R_mang")),
									      Attribute(Name("manageName"))),
							    AttrAlias(Name("A_manageName"))))),
			  TableList(Set(InnerJoin(AliasedResource(Relation(Name("Employee")),RelAlias(Name("R_emp")))),
					LeftOuterJoin(AliasedResource(Relation(Name("Manage")),RelAlias(Name("R_mang"))),
						      RelationalExpressionEq(PrimaryExpressionAttr(RelAliasAttribute(RelAlias(Name("R_mang")),Attribute(Name("emp")))),
									     PrimaryExpressionAttr(RelAliasAttribute(RelAlias(Name("R_emp")),Attribute(Name("id"))))
									   )))),
			  Some(
			      RelationalExpressionNotNull(PrimaryExpressionAttr(RelAliasAttribute(RelAlias(Name("R_emp")),Attribute(Name("lastName")))))
			    ))
    assert(expected === (a.parseAll(a.select, e).get))
  }

  test("parse LEFT OUTER SELECT") {
    val a = Sql()
    val e = """
SELECT R_emp.lastName AS A_empName, R_mang.manageName AS A_manageName
       FROM Employee AS R_emp
            LEFT OUTER JOIN (
    SELECT R_emp.lastName AS A_empName, R_mang.manageName AS A_manageName
       FROM Employee AS R_emp
            ) AS R_mang ON R_mang.emp=R_emp.id
 WHERE R_emp.lastName IS NOT NULL
"""
    val expected = 
      Select(AttributeList(Set(NamedAttribute(RelAliasAttribute(RelAlias(Name("R_emp")),Attribute(Name("lastName"))),AttrAlias(Name("A_empName"))),
			       NamedAttribute(RelAliasAttribute(RelAlias(Name("R_mang")),Attribute(Name("manageName"))),AttrAlias(Name("A_manageName"))))),
	     TableList(Set(InnerJoin(AliasedResource(Relation(Name("Employee")),RelAlias(Name("R_emp")))),
			   LeftOuterJoin(AliasedResource(
			     Subselect(Select(AttributeList(Set(NamedAttribute(RelAliasAttribute(RelAlias(Name("R_emp")),Attribute(Name("lastName"))),AttrAlias(Name("A_empName"))),
						      NamedAttribute(RelAliasAttribute(RelAlias(Name("R_mang")),Attribute(Name("manageName"))),AttrAlias(Name("A_manageName"))))),
				    TableList(Set(InnerJoin(AliasedResource(Relation(Name("Employee")),RelAlias(Name("R_emp")))))),
				    None)),
			     RelAlias(Name("R_mang"))),
					 RelationalExpressionEq(PrimaryExpressionAttr(RelAliasAttribute(RelAlias(Name("R_mang")),Attribute(Name("emp")))),
								PrimaryExpressionAttr(RelAliasAttribute(RelAlias(Name("R_emp")),Attribute(Name("id")))))))),
	     Some(RelationalExpressionNotNull(PrimaryExpressionAttr(RelAliasAttribute(RelAlias(Name("R_emp")),Attribute(Name("lastName")))))))
    assert(expected === (a.parseAll(a.select, e).get))
  }


}
