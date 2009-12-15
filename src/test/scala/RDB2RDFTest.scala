package w3c.sw

import org.scalatest.FunSuite
import java.net.URI

class RDB2RDFTest extends FunSuite {

  val db:DatabaseDesc = DatabaseDesc(
    Map(Relation("Employee") -> 
	RelationDesc(Attribute("id"), 
		     Map(Attribute("id") -> Value(SQLDatatype.INTEGER),
			 Attribute("lastName") -> Value(SQLDatatype.STRING),
			 Attribute("width") -> Value(SQLDatatype.INTEGER), 
			 Attribute("manager") -> ForeignKey(Relation("Employee"), Attribute("id")), 
			 Attribute("address") -> ForeignKey(Relation("Address"),  Attribute("id"))))))


  test("?s <p> <x>") {
    val sparqlParser = Sparql()
    val sparqlSelect = sparqlParser.parseAll(sparqlParser.select, """
SELECT ?emp {
?emp  <http://hr.example/DB/Employee#manager>    <http://hr.example/DB/Employee/id.18#record>
}
""").get
    val sqlParser = Sql()
    val sqlSelect = sqlParser.parseAll(sqlParser.select, """
SELECT R_emp.id AS A_emp
       FROM Employee AS R_emp
            INNER JOIN Employee AS R_id18 ON R_id18.id=R_emp.manager AND R_id18.id=18
""").get
    assert(RDB2RDF(db, sparqlSelect, StemURI("http://hr.example/DB/"), PrimaryKey(Attribute(Name("id")))) === sqlSelect)
    true
  }

  test("?s <p> 18") {
    /* Literal foreign keys should probably throw an error,
     * instead does what user meant. */
    val sparqlParser = Sparql()
    val sparqlSelect = sparqlParser.parseAll(sparqlParser.select, """
SELECT ?emp {
?emp  <http://hr.example/DB/Employee#manager>    "18"^^<http://www.w3.org/2001/XMLSchema#integer>
}
""").get
    val sqlParser = Sql()
    val sqlSelect = sqlParser.parseAll(sqlParser.select, """
SELECT R_emp.id AS A_emp
       FROM Employee AS R_emp
            INNER JOIN Employee AS R_18 ON R_18.id=R_emp.manager AND R_18.id=18
""").get
    assert(RDB2RDF(db, sparqlSelect, StemURI("http://hr.example/DB/"), PrimaryKey(Attribute(Name("id")))) === sqlSelect)
    true
  }

  test("transform SQLbgp") {
    val sparqlParser = Sparql()
    val sparqlSelect = sparqlParser.parseAll(sparqlParser.select, """
SELECT ?empName ?manageName {
?emp      <http://hr.example/DB/Employee#lastName>   ?empName .
?emp      <http://hr.example/DB/Employee#manager>    ?manager .
?manager  <http://hr.example/DB/Employee#lastName>   ?manageName 
}
""").get
    val sqlParser = Sql()
    val sqlSelect = sqlParser.parseAll(sqlParser.select, """
SELECT R_emp.lastName AS A_empName, R_manager.lastName AS A_manageName
       FROM Employee AS R_emp
            INNER JOIN Employee AS R_manager ON R_manager.id=R_emp.manager
 WHERE R_emp.lastName IS NOT NULL AND R_manager.lastName IS NOT NULL
""").get
    assert(RDB2RDF(db, sparqlSelect, StemURI("http://hr.example/DB/"), PrimaryKey(Attribute(Name("id")))) === sqlSelect)
  }

  test("transform tup1") {
    val sparqlParser = Sparql()
    val sparqlSelect = sparqlParser.parseAll(sparqlParser.select, """
SELECT ?empName {
 ?emp      <http://hr.example/DB/Employee#lastName>   ?empName .
 ?emp      <http://hr.example/DB/Employee#manager>    <http://hr.example/DB/Employee/id.18#record>
 }
""").get
    val sqlParser = Sql()
    val sqlSelect = sqlParser.parseAll(sqlParser.select, """
SELECT R_emp.lastName AS A_empName
       FROM Employee AS R_emp
            INNER JOIN Employee AS R_id18 ON R_id18.id=R_emp.manager AND R_id18.id=18
 WHERE R_emp.lastName IS NOT NULL
""").get
    assert(RDB2RDF(db, sparqlSelect, StemURI("http://hr.example/DB/"), PrimaryKey(Attribute(Name("id")))) === sqlSelect)
  }


  test("transform litConst1") {
    val sparqlParser = Sparql()
    val sparqlSelect = sparqlParser.parseAll(sparqlParser.select, """
SELECT ?empName {
 ?emp      <http://hr.example/DB/Employee#lastName>   ?empName .
 ?emp      <http://hr.example/DB/Employee#manager>    ?manager .
 ?manager  <http://hr.example/DB/Employee#lastName>   "Johnson"^^<http://www.w3.org/2001/XMLSchema#string>
 }
""").get
    val sqlParser = Sql()
    val sqlSelect = sqlParser.parseAll(sqlParser.select, """
SELECT R_emp.lastName AS A_empName
  FROM Employee AS R_emp
       INNER JOIN Employee AS R_manager ON R_manager.id=R_emp.manager
WHERE R_manager.lastName="Johnson" AND R_emp.lastName IS NOT NULL
""").get
    assert(RDB2RDF(db, sparqlSelect, StemURI("http://hr.example/DB/"), PrimaryKey(Attribute(Name("id")))) === sqlSelect)
  }

}
