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
      RelationalExpressionEq(RelAliasAttribute(RelAlias(Name("R_manager")),Attribute(Name("id"))),
			     RValueAttr(RelAliasAttribute(RelAlias(Name("R_emp")),Attribute(Name("manager"))))),
      RelationalExpressionNotNull(RelAliasAttribute(RelAlias(Name("R_emp")),Attribute(Name("lastName")))),
      RelationalExpressionNotNull(RelAliasAttribute(RelAlias(Name("R_manager")),Attribute(Name("lastName"))))))
    assert(expected === (a.parseAll(a.expression, e).get))
  }

  test("parse ORexpression") {
    // AliasedResource(Relation(Name("Employee")),RelAlias(Name("R_emp")))
    val a = Sql()
    val e = """
R_manager.id=R_emp.manager OR R_emp.lastName IS NOT NULL OR R_manager.lastName IS NOT NULL
"""
    val expected = ExprDisjunction(Set(
      RelationalExpressionEq(RelAliasAttribute(RelAlias(Name("R_manager")),Attribute(Name("id"))),
			     RValueAttr(RelAliasAttribute(RelAlias(Name("R_emp")),Attribute(Name("manager"))))),
      RelationalExpressionNotNull(RelAliasAttribute(RelAlias(Name("R_emp")),Attribute(Name("lastName")))),
      RelationalExpressionNotNull(RelAliasAttribute(RelAlias(Name("R_manager")),Attribute(Name("lastName"))))))
    assert(expected === (a.parseAll(a.expression, e).get))
  }

  test("parse nested expression") {
    // AliasedResource(Relation(Name("Employee")),RelAlias(Name("R_emp")))
    val a = Sql()
    val e = """
( R_manager.id=R_emp.manager OR R_emp.lastName IS NOT NULL OR R_manager.lastName IS NOT NULL )
"""
    val expected = ExprDisjunction(Set(
      RelationalExpressionEq(RelAliasAttribute(RelAlias(Name("R_manager")),Attribute(Name("id"))),
			     RValueAttr(RelAliasAttribute(RelAlias(Name("R_emp")),Attribute(Name("manager"))))),
      RelationalExpressionNotNull(RelAliasAttribute(RelAlias(Name("R_emp")),Attribute(Name("lastName")))),
      RelationalExpressionNotNull(RelAliasAttribute(RelAlias(Name("R_manager")),Attribute(Name("lastName"))))))
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
			  TableList(Set(AliasedResource(Relation(Name("Employee")),RelAlias(Name("R_emp"))),
					 AliasedResource(Relation(Name("Employee")),RelAlias(Name("R_manager"))))),
			  Some(ExprConjunction(Set(
			    RelationalExpressionEq(RelAliasAttribute(RelAlias(Name("R_manager")),Attribute(Name("id"))),
						RValueAttr(RelAliasAttribute(RelAlias(Name("R_emp")),Attribute(Name("manager"))))),
			    RelationalExpressionNotNull(RelAliasAttribute(RelAlias(Name("R_emp")),Attribute(Name("lastName")))),
			    RelationalExpressionNotNull(RelAliasAttribute(RelAlias(Name("R_manager")),Attribute(Name("lastName"))))))))
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
			  Some(ExprConjunction(Set(RelationalExpressionEq(RelAliasAttribute(RelAlias(Name("R_emp")),Attribute(Name("manager"))),
							      RValueTyped(SQLDatatype.INTEGER,Name("18"))),
					  RelationalExpressionNotNull(RelAliasAttribute(RelAlias(Name("R_emp")),Attribute(Name("lastName"))))))))
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
			  Some(ExprConjunction(Set(RelationalExpressionEq(RelAliasAttribute(RelAlias(Name("R_emp")),Attribute(Name("manager"))),
							      RValueAttr(RelAliasAttribute(RelAlias(Name("R_manager")),Attribute(Name("id"))))),
					  RelationalExpressionEq(RelAliasAttribute(RelAlias(Name("R_manager")),Attribute(Name("lastName"))),
							      RValueTyped(SQLDatatype.STRING,Name("Johnson"))),
					  RelationalExpressionNotNull(RelAliasAttribute(RelAlias(Name("R_emp")),Attribute(Name("lastName"))))))))
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
			  Some(ExprConjunction(Set(RelationalExpressionEq(RelAliasAttribute(RelAlias(Name("R_lower")),Attribute(Name("manages"))),
							     RValueAttr(RelAliasAttribute(RelAlias(Name("R_emp")),Attribute(Name("id"))))),
					 RelationalExpressionEq(RelAliasAttribute(RelAlias(Name("R_manager")),Attribute(Name("id"))),
							     RValueAttr(RelAliasAttribute(RelAlias(Name("R_lower")),Attribute(Name("manager"))))),
					 RelationalExpressionLt(RelAliasAttribute(RelAlias(Name("R_manager")),Attribute(Name("birthday"))),
							     RValueAttr(RelAliasAttribute(RelAlias(Name("R_emp")),Attribute(Name("birthday"))))),
					 RelationalExpressionEq(RelAliasAttribute(RelAlias(Name("R_upper")),Attribute(Name("manages"))),
							     RValueAttr(RelAliasAttribute(RelAlias(Name("R_manager")),Attribute(Name("id"))))),
					 RelationalExpressionEq(RelAliasAttribute(RelAlias(Name("R_grandManager")),Attribute(Name("id"))),
							     RValueAttr(RelAliasAttribute(RelAlias(Name("R_upper")),Attribute(Name("manager"))))),
					 RelationalExpressionLt(RelAliasAttribute(RelAlias(Name("R_grandManager")),Attribute(Name("birthday"))),
							     RValueAttr(RelAliasAttribute(RelAlias(Name("R_manager")),Attribute(Name("birthday"))))),
					 RelationalExpressionNotNull(RelAliasAttribute(RelAlias(Name("R_emp")),Attribute(Name("lastName")))),
					 RelationalExpressionNotNull(RelAliasAttribute(RelAlias(Name("R_grandManager")),Attribute(Name("lastName"))))))))
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
						 Some(ExprConjunction(Set(RelationalExpressionEq(RelAliasAttribute(RelAlias(Name("R_above")),Attribute(Name("manager"))),
										    RValueAttr(RelAliasAttribute(RelAlias(Name("R_manager")),Attribute(Name("id"))))),
								RelationalExpressionNotNull(RelAliasAttribute(RelAlias(Name("R_manager")),Attribute(Name("lastName")))))))), 
					  Select(AttributeList(Set(NamedAttribute(RelAliasAttribute(RelAlias(Name("R_managed")), Attribute(Name("lastName"))),
										  AttrAlias(Name("A_name"))), 
								   NamedAttribute(RelAliasAttribute(RelAlias(Name("R_below")), Attribute(Name("manager"))),
										  AttrAlias(Name("A_who"))))),
						 TableList(Set(
						   AliasedResource(Relation(Name("Manage")),RelAlias(Name("R_below"))),
						   AliasedResource(Relation(Name("Employee")),RelAlias(Name("R_managed")))
						 )), 
						 Some(ExprConjunction(Set(RelationalExpressionEq(RelAliasAttribute(RelAlias(Name("R_below")),Attribute(Name("manages"))),
										    RValueAttr(RelAliasAttribute(RelAlias(Name("R_managed")),Attribute(Name("id"))))),
								RelationalExpressionNotNull(RelAliasAttribute(RelAlias(Name("R_managed")),Attribute(Name("lastName"))))))))))),
							RelAlias(Name("R_union1"))))), 
			  Some(ExprConjunction(Set(RelationalExpressionEq(RelAliasAttribute(RelAlias(Name("R_union1")),Attribute(Name("A_who"))),
							     RValueAttr(RelAliasAttribute(RelAlias(Name("R_who")),Attribute(Name("id"))))),
					 RelationalExpressionEq(RelAliasAttribute(RelAlias(Name("R_who")),Attribute(Name("lastName"))),
							     RValueTyped(SQLDatatype.STRING,Name("Smith")))))))
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
			  TableList(Set(AliasedResource(Relation(Name("Manage")),RelAlias(Name("R_above"))))),
			  Some(
			    RelationalExpressionNotNull(RelAliasAttribute(RelAlias(Name("R_above")),Attribute(Name("id"))))))
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
			  TableList(Set(AliasedResource(Relation(Name("Manage")),RelAlias(Name("R_above"))))),
			  Some(
			    ExprDisjunction(Set(
			      RelationalExpressionNotNull(RelAliasAttribute(RelAlias(Name("R_above")),Attribute(Name("id")))),
			      ExprConjunction(Set(
				RelationalExpressionLt(RelAliasAttribute(RelAlias(Name("R_above")),Attribute(Name("id"))),
						       RValueTyped(SQLDatatype.INTEGER,Name("5"))),
				RelationalExpressionLt(RelAliasAttribute(RelAlias(Name("R_above")),Attribute(Name("id"))),
						       RValueTyped(SQLDatatype.INTEGER,Name("3")))
			      ))))))
    assert(expected === (a.parseAll(a.select, e).get))
  }

}
