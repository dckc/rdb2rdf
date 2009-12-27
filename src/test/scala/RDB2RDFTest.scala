package w3c.sw

import org.scalatest.FunSuite
import java.net.URI

class RDB2RDFTest extends FunSuite {

  val db:DatabaseDesc = DatabaseDesc(
    Map(Relation("Employee") -> 
	RelationDesc(Option(Attribute("id")), 
		     Map(Attribute("id") -> Value(SQLDatatype.INTEGER),
			 Attribute("lastName") -> Value(SQLDatatype.STRING),
			 Attribute("birthday") -> Value(SQLDatatype.INTEGER), // !!!
			 Attribute("manager") -> ForeignKey(Relation("Employee"), Attribute("id")), 
			 Attribute("address") -> ForeignKey(Relation("Address"),  Attribute("id"))))
      ))

  val db2:DatabaseDesc = DatabaseDesc(
    Map(Relation("Employee") -> 
	RelationDesc(Option(Attribute("id")), 
		     Map(Attribute("id") -> Value(SQLDatatype.INTEGER),
			 Attribute("lastName") -> Value(SQLDatatype.STRING),
			 Attribute("birthday") -> Value(SQLDatatype.INTEGER), // !!!
			 Attribute("manager") -> Value(SQLDatatype.INTEGER),
			 Attribute("address") -> Value(SQLDatatype.INTEGER))),
	Relation("Manage") -> 
	RelationDesc(None,
		     Map(Attribute("manager") -> ForeignKey(Relation("Employee"), Attribute("id")), 
			 Attribute("manages") -> ForeignKey(Relation("Employee"),  Attribute("id"))))
      ))


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
            INNER JOIN Employee AS R_id18
 WHERE R_id18.id=R_emp.manager AND R_id18.id=18 AND R_emp.id IS NOT NULL
""").get
    assert(RDB2RDF(db, sparqlSelect, StemURI("http://hr.example/DB/"), PrimaryKey(Attribute(Name("id")))) === sqlSelect)
    true
  }

  test("<s> <p> ?x") {
    val sparqlParser = Sparql()
    val sparqlSelect = sparqlParser.parseAll(sparqlParser.select, """
SELECT ?manager {
<http://hr.example/DB/Employee/id.18#record>  <http://hr.example/DB/Employee#manager>    ?manager
}
""").get
    val sqlParser = Sql()
    val sqlSelect = sqlParser.parseAll(sqlParser.select, """
SELECT R_manager.id AS A_manager
       FROM Employee AS R_id18
            INNER JOIN Employee AS R_manager
 WHERE R_manager.id=R_id18.manager AND R_id18.id=18 AND R_manager.id IS NOT NULL
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
            INNER JOIN Employee AS R_18
 WHERE R_18.id=R_emp.manager AND R_18.id=18 AND R_emp.id IS NOT NULL
""").get
    assert(RDB2RDF(db, sparqlSelect, StemURI("http://hr.example/DB/"), PrimaryKey(Attribute(Name("id")))) === sqlSelect)
    true
  }

  test("?s1 <p> ?x . ?s2 <p> ?x") {
    val sparqlParser = Sparql()
    val sparqlSelect = sparqlParser.parseAll(sparqlParser.select, """
SELECT ?emp1 ?emp2 ?sharedName {
   ?emp1  <http://hr.example/DB/Employee#lastName>    ?sharedName .
   ?emp2  <http://hr.example/DB/Employee#lastName>    ?sharedName
}
""").get
    val sqlParser = Sql()
    val sqlSelect = sqlParser.parseAll(sqlParser.select, """
SELECT R_emp1.id AS A_emp1, R_emp2.id AS A_emp2, R_emp1.lastName AS A_sharedName
       FROM Employee AS R_emp1
            INNER JOIN Employee AS R_emp2
 WHERE R_emp1.lastName=R_emp2.lastName AND R_emp1.id IS NOT NULL AND R_emp1.lastName IS NOT NULL AND R_emp2.id IS NOT NULL
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
            INNER JOIN Employee AS R_manager
 WHERE R_manager.id=R_emp.manager AND R_emp.lastName IS NOT NULL AND R_manager.lastName IS NOT NULL
 AND R_emp.id IS NOT NULL
 AND R_manager.id IS NOT NULL
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
            INNER JOIN Employee AS R_id18
 WHERE R_id18.id=R_emp.manager AND R_id18.id=18 AND R_emp.lastName IS NOT NULL
 AND R_emp.id IS NOT NULL
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
       INNER JOIN Employee AS R_manager
WHERE R_manager.id=R_emp.manager AND R_manager.lastName="Johnson" AND R_emp.lastName IS NOT NULL
 AND R_emp.id IS NOT NULL
 AND R_manager.id IS NOT NULL
""").get
    assert(RDB2RDF(db, sparqlSelect, StemURI("http://hr.example/DB/"), PrimaryKey(Attribute(Name("id")))) === sqlSelect)
  }

  test("transform filter1") {
    val sparqlParser = Sparql()
    val sparqlSelect = sparqlParser.parseAll(sparqlParser.select, """
SELECT ?empName ?grandManagName {
         ?emp          <http://hr.example/DB/Employee#lastName>   ?empName .
         ?emp          <http://hr.example/DB/Employee#birthday>   ?empBday .
         ?lower        <http://hr.example/DB/Manage#manages>   ?emp .
         ?lower        <http://hr.example/DB/Manage#manager>   ?manager .
         ?manager      <http://hr.example/DB/Employee#birthday>   ?manBday .
         ?upper        <http://hr.example/DB/Manage#manages>   ?manager .
         ?upper        <http://hr.example/DB/Manage#manager>   ?grandManager .
         ?grandManager <http://hr.example/DB/Employee#birthday>   ?grandManBday .
         ?grandManager <http://hr.example/DB/Employee#lastName>   ?grandManagName
         FILTER (?manBday < ?empBday && ?grandManBday < ?manBday)
}
""").get
    val sqlParser = Sql()
    val sqlSelect = sqlParser.parseAll(sqlParser.select, """
SELECT R_emp.lastName AS A_empName, R_grandManager.lastName AS A_grandManagName
  FROM Employee AS R_emp
       INNER JOIN Manage AS R_lower
       INNER JOIN Employee AS R_manager
       INNER JOIN Manage AS R_upper
       INNER JOIN Employee AS R_grandManager
 WHERE R_emp.id=R_lower.manages AND R_manager.id=R_lower.manager AND R_manager.id=R_upper.manages AND R_grandManager.id=R_upper.manager AND R_manager.birthday < R_emp.birthday AND R_grandManager.birthday < R_manager.birthday AND R_emp.lastName IS NOT NULL AND R_grandManager.lastName IS NOT NULL
 AND R_emp.id IS NOT NULL
 AND R_lower.id IS NOT NULL
 AND R_manager.id IS NOT NULL
 AND R_upper.id IS NOT NULL
 AND R_grandManager.id IS NOT NULL
 AND R_emp.birthday IS NOT NULL
 AND R_manager.birthday IS NOT NULL
 AND R_grandManager.birthday IS NOT NULL
""").get
    assert(RDB2RDF(db2, sparqlSelect, StemURI("http://hr.example/DB/"), PrimaryKey(Attribute(Name("id")))) === sqlSelect)
  }

//   test("transform disj1") {
//     val sparqlParser = Sparql()
//     val sparqlSelect = sparqlParser.parseAll(sparqlParser.select, """
// SELECT ?name
//  WHERE { ?who emplP:lastName "Smith"
//          { ?above   manageP:manages ?who .
//            ?above   manageP:manager ?manager .
//            ?manager emplP:lastName  ?name }
//          UNION
//          { ?below   manageP:manager ?who .
//            ?below   manageP:manages ?managed .
//            ?managed emplP:lastName  ?name } }
// """).get
//     val sqlParser = Sql()
//     val sqlSelect = sqlParser.parseAll(sqlParser.select, """
// SELECT union1.name
//   FROM Employee AS who
//        INNER JOIN (
//          SELECT manager.lastName AS name, above.manages AS who
//                 FROM Manage AS above
//                 INNER JOIN Employee as manager ON above.manager=manager.id
//           WHERE manager.lastName IS NOT NULL
//        UNION
//          SELECT managed.lastName AS name, below.manager AS who
//                 FROM Manage AS below
//                 INNER JOIN Employee as managed ON below.manages=managed.id
//           WHERE managed.lastName IS NOT NULL
//        ) AS union1 ON union1.who=who.id
//  WHERE who.lastName="Smith"
// """).get
//     assert(RDB2RDF(db2, sparqlSelect, StemURI("http://hr.example/DB/"), PrimaryKey(Attribute(Name("id")))) === sqlSelect)
//   }
}
