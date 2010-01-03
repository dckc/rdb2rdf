package w3c.sw

import org.scalatest.FunSuite
import java.net.URI

class RDB2RDFTest extends FunSuite {

  val db:DatabaseDesc = DatabaseDesc(
    Map(Relation("Employee") -> 
	RelationDesc(Option(Attribute("id")), 
		     Map(Attribute("id") -> Value(SQLDatatype.INTEGER),
			 Attribute("lastName") -> Value(SQLDatatype.STRING),
			 Attribute("birthday") -> Value(SQLDatatype.DATE),
			 Attribute("manager") -> ForeignKey(Relation("Employee"), Attribute("id")), 
			 Attribute("address") -> ForeignKey(Relation("Address"),  Attribute("id"))))
      ))

  val db2:DatabaseDesc = DatabaseDesc(
    Map(Relation("Employee") -> 
	RelationDesc(Option(Attribute("id")), 
		     Map(Attribute("id") -> Value(SQLDatatype.INTEGER),
			 Attribute("lastName") -> Value(SQLDatatype.STRING),
			 Attribute("birthday") -> Value(SQLDatatype.DATE),
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
PREFIX empP : <http://hr.example/DB/Employee#>
SELECT ?emp {
?emp  empP:manager    <http://hr.example/DB/Employee/id.18#record>
}
""").get
    val sqlParser = Sql()
    val sqlSelect = sqlParser.parseAll(sqlParser.select, """
SELECT R_emp.id AS A_emp
       FROM Employee AS R_emp
            INNER JOIN Employee AS R_id18
 WHERE R_id18.id=R_emp.manager AND R_id18.id=18 AND R_emp.id IS NOT NULL
""").get
    assert(RDB2RDF(db, sparqlSelect, StemURI("http://hr.example/DB/"), PrimaryKey(Attribute(Name("id"))), true) === sqlSelect)
    true
  }

  test("<s> <p> ?x") {
    val sparqlParser = Sparql()
    val sparqlSelect = sparqlParser.parseAll(sparqlParser.select, """
PREFIX empP : <http://hr.example/DB/Employee#>
SELECT ?manager {
<http://hr.example/DB/Employee/id.18#record>  empP:manager    ?manager
}
""").get
    val sqlParser = Sql()
    val sqlSelect = sqlParser.parseAll(sqlParser.select, """
SELECT R_manager.id AS A_manager
       FROM Employee AS R_id18
            INNER JOIN Employee AS R_manager
 WHERE R_manager.id=R_id18.manager AND R_id18.id=18 AND R_manager.id IS NOT NULL
""").get
    assert(RDB2RDF(db, sparqlSelect, StemURI("http://hr.example/DB/"), PrimaryKey(Attribute(Name("id"))), true) === sqlSelect)
    true
  }

  test("?s <p> 18") {
    /* Literal foreign keys should probably throw an error,
     * instead does what user meant. */
    val sparqlParser = Sparql()
    val sparqlSelect = sparqlParser.parseAll(sparqlParser.select, """
PREFIX empP : <http://hr.example/DB/Employee#>
PREFIX xsd : <http://www.w3.org/2001/XMLSchema#>
SELECT ?emp {
?emp  empP:manager    "18"^^xsd:integer
}
""").get
    val sqlParser = Sql()
    val sqlSelect = sqlParser.parseAll(sqlParser.select, """
SELECT R_emp.id AS A_emp
       FROM Employee AS R_emp
            INNER JOIN Employee AS R_18
 WHERE R_18.id=R_emp.manager AND R_18.id=18 AND R_emp.id IS NOT NULL
""").get
    assert(RDB2RDF(db, sparqlSelect, StemURI("http://hr.example/DB/"), PrimaryKey(Attribute(Name("id"))), true) === sqlSelect)
    true
  }

  test("?s1 <p> ?x . ?s2 <p> ?x") {
    val sparqlParser = Sparql()
    val sparqlSelect = sparqlParser.parseAll(sparqlParser.select, """
PREFIX empP : <http://hr.example/DB/Employee#>
SELECT ?emp1 ?emp2 ?sharedName {
   ?emp1  empP:lastName    ?sharedName .
   ?emp2  empP:lastName    ?sharedName
}
""").get
    val sqlParser = Sql()
    val sqlSelect = sqlParser.parseAll(sqlParser.select, """
SELECT R_emp1.id AS A_emp1, R_emp2.id AS A_emp2, R_emp1.lastName AS A_sharedName
       FROM Employee AS R_emp1
            INNER JOIN Employee AS R_emp2
 WHERE R_emp1.lastName=R_emp2.lastName AND R_emp1.id IS NOT NULL AND R_emp1.lastName IS NOT NULL AND R_emp2.id IS NOT NULL
""").get
    assert(RDB2RDF(db, sparqlSelect, StemURI("http://hr.example/DB/"), PrimaryKey(Attribute(Name("id"))), true) === sqlSelect)
    true
  }

  test("transform SQLbgp") {
    val sparqlParser = Sparql()
    val sparqlSelect = sparqlParser.parseAll(sparqlParser.select, """
PREFIX empP : <http://hr.example/DB/Employee#>
SELECT ?empName ?manageName {
?emp      empP:lastName   ?empName .
?emp      empP:manager    ?manager .
?manager  empP:lastName   ?manageName 
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
    assert(RDB2RDF(db, sparqlSelect, StemURI("http://hr.example/DB/"), PrimaryKey(Attribute(Name("id"))), true) === sqlSelect)
  }

  test("transform tup1 no-enforce") {
    val sparqlParser = Sparql()
    val sparqlSelect = sparqlParser.parseAll(sparqlParser.select, """
PREFIX empP : <http://hr.example/DB/Employee#>
SELECT ?empName {
 ?emp      empP:lastName   ?empName .
 ?emp      empP:manager    <http://hr.example/DB/Employee/id.18#record>
 }
""").get
    val sqlParser = Sql()
    val sqlSelect = sqlParser.parseAll(sqlParser.select, """
SELECT R_emp.lastName AS A_empName
       FROM Employee AS R_emp
 WHERE R_emp.manager=18 AND R_emp.lastName IS NOT NULL
 AND R_emp.id IS NOT NULL
""").get
    assert(RDB2RDF(db, sparqlSelect, StemURI("http://hr.example/DB/"), PrimaryKey(Attribute(Name("id"))), false) === sqlSelect)
  }

  test("transform tup1 enforce") {
    val sparqlParser = Sparql()
    val sparqlSelect = sparqlParser.parseAll(sparqlParser.select, """
PREFIX empP : <http://hr.example/DB/Employee#>
SELECT ?empName {
 ?emp      empP:lastName   ?empName .
 ?emp      empP:manager    <http://hr.example/DB/Employee/id.18#record>
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
    assert(RDB2RDF(db, sparqlSelect, StemURI("http://hr.example/DB/"), PrimaryKey(Attribute(Name("id"))), true) === sqlSelect)
  }


  test("transform litConst1") {
    val sparqlParser = Sparql()
    val sparqlSelect = sparqlParser.parseAll(sparqlParser.select, """
PREFIX empP : <http://hr.example/DB/Employee#>
PREFIX xsd : <http://www.w3.org/2001/XMLSchema#>
SELECT ?empName {
 ?emp      empP:lastName   ?empName .
 ?emp      empP:manager    ?manager .
 ?manager  empP:lastName   "Johnson"^^xsd:string
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
    assert(RDB2RDF(db, sparqlSelect, StemURI("http://hr.example/DB/"), PrimaryKey(Attribute(Name("id"))), true) === sqlSelect)
  }

  test("transform filter1") {
    val sparqlParser = Sparql()
    val sparqlSelect = sparqlParser.parseAll(sparqlParser.select, """
PREFIX empP : <http://hr.example/DB/Employee#>
PREFIX manP : <http://hr.example/DB/Manage#>
SELECT ?empName ?grandManagName {
         ?emp          empP:lastName   ?empName .
         ?emp          empP:birthday   ?empBday .
         ?lower        manP:manages   ?emp .
         ?lower        manP:manager   ?manager .
         ?manager      empP:birthday   ?manBday .
         ?upper        manP:manages   ?manager .
         ?upper        manP:manager   ?grandManager .
         ?grandManager empP:birthday   ?grandManBday .
         ?grandManager empP:lastName   ?grandManagName
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
    assert(RDB2RDF(db2, sparqlSelect, StemURI("http://hr.example/DB/"), PrimaryKey(Attribute(Name("id"))), true) === sqlSelect)
  }

  test("transform disj1") {
    val sparqlParser = Sparql()
    val sparqlSelect = sparqlParser.parseAll(sparqlParser.select, """
PREFIX empP : <http://hr.example/DB/Employee#>
PREFIX manP : <http://hr.example/DB/Manage#>
SELECT ?name
 WHERE { ?who empP:lastName "Smith"^^xsd:string
         { ?above   manP:manages ?who .
           ?above   manP:manager ?manager .
           ?manager empP:lastName  ?name }
         UNION
         { ?below   manP:manager ?who .
           ?below   manP:manages ?managed .
           ?managed empP:lastName  ?name } }
""").get
    val sqlParser = Sql()
    val sqlSelect = sqlParser.parseAll(sqlParser.select, """
SELECT R_union1.A_name AS A_name
  FROM Employee AS R_who
       INNER JOIN (
         SELECT 0 AS _DISJOINT_, R_manager.lastName AS A_name, R_above.manager AS A_manager, 
                R_above.id AS A_above, NULL AS A_below, R_above.manages AS A_who, NULL AS A_managed
                FROM Manage AS R_above
                INNER JOIN Employee AS R_manager
          WHERE R_above.manager=R_manager.id AND R_manager.lastName IS NOT NULL
                AND R_above.manager IS NOT NULL AND R_above.id IS NOT NULL AND R_above.manages IS NOT NULL
       UNION
         SELECT 1 AS _DISJOINT_, R_managed.lastName AS A_name, NULL AS A_manager, 
                NULL AS A_above, R_below.id AS A_below, R_below.manager AS A_who, R_below.manages AS A_managed
                FROM Manage AS R_below
                INNER JOIN Employee AS R_managed
          WHERE R_below.manages=R_managed.id AND R_managed.lastName IS NOT NULL
                AND R_below.manager IS NOT NULL AND R_below.id IS NOT NULL AND R_below.manages IS NOT NULL
       ) AS R_union1
 WHERE R_who.lastName="Smith" AND R_who.id IS NOT NULL AND
       (R_union1._DISJOINT_!=0 OR R_who.id=R_union1.A_who) AND
       (R_union1._DISJOINT_!=1 OR R_who.id=R_union1.A_who)
""").get
    assert(RDB2RDF(db2, sparqlSelect, StemURI("http://hr.example/DB/"), PrimaryKey(Attribute(Name("id"))), false) === sqlSelect)
  }

  test("transform assymDisj1") {
    val sparqlParser = Sparql()
    val sparqlSelect = sparqlParser.parseAll(sparqlParser.select, """
PREFIX empP : <http://hr.example/DB/Employee#>
PREFIX manP : <http://hr.example/DB/Manage#>
PREFIX xsd : <http://www.w3.org/2001/XMLSchema#>
SELECT ?name
 WHERE { { ?above   manP:manages ?who .
           ?above   manP:manager ?manager .
           ?manager empP:lastName  ?name }
         UNION
         { ?below   manP:manager ?who .
           ?below   manP:manages ?managed .
           ?managed empP:lastName  ?name .
           ?managed empP:birthday  ?bday } 
         ?who empP:lastName "Smith"^^xsd:string .
         ?who empP:birthday ?bday }
""").get
    val sqlParser = Sql()
    val sqlSelect = sqlParser.parseAll(sqlParser.select, """
SELECT R_union0.A_name AS A_name
  FROM ( SELECT 0 AS _DISJOINT_, R_above.manager AS A_manager, R_manager.lastName AS A_name, R_above.id AS A_above, 
                NULL AS A_below, NULL AS A_bday, R_above.manages AS A_who, NULL AS A_managed
                FROM Manage AS R_above
                INNER JOIN Employee AS R_manager
          WHERE R_above.manager IS NOT NULL AND R_above.manager=R_manager.id AND R_above.id IS NOT NULL
                AND R_above.manages IS NOT NULL AND R_manager.lastName IS NOT NULL
       UNION
         SELECT 1 AS _DISJOINT_, NULL AS A_manager, R_managed.lastName AS A_name, NULL AS A_above, 
                R_below.id AS A_below, R_managed.birthday AS A_bday, R_below.manager AS A_who, R_below.manages AS A_managed
                FROM Manage AS R_below
                INNER JOIN Employee AS R_managed
          WHERE R_managed.birthday IS NOT NULL AND R_below.manager IS NOT NULL AND R_below.id IS NOT NULL
                AND R_below.manages=R_managed.id AND R_below.manages IS NOT NULL AND R_managed.lastName IS NOT NULL
       ) AS R_union0
       INNER JOIN Employee AS R_who
 WHERE R_who.lastName="Smith" AND
       (R_union0._DISJOINT_!=0 OR R_union0.A_who=R_who.id) AND
       (R_union0._DISJOINT_!=1 OR R_union0.A_who=R_who.id) AND
       (R_union0._DISJOINT_!=1 OR R_union0.A_bday=R_who.birthday) AND
       R_union0.A_who IS NOT NULL AND
       R_union0.A_bday IS NOT NULL
""").get
    assert(RDB2RDF(db2, sparqlSelect, StemURI("http://hr.example/DB/"), PrimaryKey(Attribute(Name("id"))), false) === sqlSelect)
  }

  test("transform assymDisj1 reversed") {
    val sparqlParser = Sparql()
    val sparqlSelect = sparqlParser.parseAll(sparqlParser.select, """
PREFIX empP : <http://hr.example/DB/Employee#>
PREFIX manP : <http://hr.example/DB/Manage#>
PREFIX xsd : <http://www.w3.org/2001/XMLSchema#>
SELECT ?name
 WHERE {
         ?who empP:lastName "Smith"^^xsd:string .
         ?who empP:birthday ?bday
         { ?above   manP:manages ?who .
           ?above   manP:manager ?manager .
           ?manager empP:lastName  ?name }
         UNION
         { ?below   manP:manager ?who .
           ?below   manP:manages ?managed .
           ?managed empP:lastName  ?name .
           ?managed empP:birthday  ?bday } 
       }
""").get
    val sqlParser = Sql()
    val sqlSelect = sqlParser.parseAll(sqlParser.select, """
SELECT R_union1.A_name AS A_name
  FROM Employee AS R_who
       INNER JOIN ( SELECT 0 AS _DISJOINT_, R_above.manager AS A_manager, R_manager.lastName AS A_name, R_above.id AS A_above, 
                NULL AS A_below, NULL AS A_bday, R_above.manages AS A_who, NULL AS A_managed
                FROM Manage AS R_above
                INNER JOIN Employee AS R_manager
          WHERE R_above.manager IS NOT NULL AND R_above.manager=R_manager.id AND R_above.id IS NOT NULL
                AND R_above.manages IS NOT NULL AND R_manager.lastName IS NOT NULL
       UNION
         SELECT 1 AS _DISJOINT_, NULL AS A_manager, R_managed.lastName AS A_name, NULL AS A_above, 
                R_below.id AS A_below, R_managed.birthday AS A_bday, R_below.manager AS A_who, R_below.manages AS A_managed
                FROM Manage AS R_below
                INNER JOIN Employee AS R_managed
          WHERE R_managed.birthday IS NOT NULL AND R_below.manager IS NOT NULL AND R_below.id IS NOT NULL
                AND R_below.manages=R_managed.id AND R_below.manages IS NOT NULL AND R_managed.lastName IS NOT NULL
       ) AS R_union1
 WHERE R_who.lastName="Smith" AND
       (R_union1._DISJOINT_!=0 OR R_who.id=R_union1.A_who) AND
       (R_union1._DISJOINT_!=1 OR R_who.id=R_union1.A_who) AND
       (R_union1._DISJOINT_!=1 OR R_who.birthday=R_union1.A_bday) AND
       R_who.id IS NOT NULL AND
       R_who.birthday IS NOT NULL
""").get
    assert(RDB2RDF(db2, sparqlSelect, StemURI("http://hr.example/DB/"), PrimaryKey(Attribute(Name("id"))), false) === sqlSelect)
  }

  test("transform assymDisj1 interspersed") {
    val sparqlParser = Sparql()
    val sparqlSelect = sparqlParser.parseAll(sparqlParser.select, """
PREFIX empP : <http://hr.example/DB/Employee#>
PREFIX manP : <http://hr.example/DB/Manage#>
PREFIX xsd : <http://www.w3.org/2001/XMLSchema#>
SELECT ?name
 WHERE {
         ?who empP:lastName "Smith"^^xsd:string
         { ?above   manP:manages ?who .
           ?above   manP:manager ?manager .
           ?manager empP:lastName  ?name }
         UNION
         { ?below   manP:manager ?who .
           ?below   manP:manages ?managed .
           ?managed empP:lastName  ?name .
           ?managed empP:birthday  ?bday } 
         ?who empP:birthday ?bday
       }
""").get
    val sqlParser = Sql()
    val sqlSelect = sqlParser.parseAll(sqlParser.select, """
SELECT R_union1.A_name AS A_name
  FROM Employee AS R_who
       INNER JOIN ( SELECT 0 AS _DISJOINT_, R_above.manager AS A_manager, R_manager.lastName AS A_name, R_above.id AS A_above, 
                NULL AS A_below, NULL AS A_bday, R_above.manages AS A_who, NULL AS A_managed
                FROM Manage AS R_above
                INNER JOIN Employee AS R_manager
          WHERE R_above.manager IS NOT NULL AND R_above.manager=R_manager.id AND R_above.id IS NOT NULL
                AND R_above.manages IS NOT NULL AND R_manager.lastName IS NOT NULL
       UNION
         SELECT 1 AS _DISJOINT_, NULL AS A_manager, R_managed.lastName AS A_name, NULL AS A_above, 
                R_below.id AS A_below, R_managed.birthday AS A_bday, R_below.manager AS A_who, R_below.manages AS A_managed
                FROM Manage AS R_below
                INNER JOIN Employee AS R_managed
          WHERE R_managed.birthday IS NOT NULL AND R_below.manager IS NOT NULL AND R_below.id IS NOT NULL
                AND R_below.manages=R_managed.id AND R_below.manages IS NOT NULL AND R_managed.lastName IS NOT NULL
       ) AS R_union1
 WHERE R_who.lastName="Smith" AND
       (R_union1._DISJOINT_!=0 OR R_who.id=R_union1.A_who) AND
       (R_union1._DISJOINT_!=1 OR R_who.id=R_union1.A_who) AND
       (R_union1._DISJOINT_!=1 OR R_union1.A_bday=R_who.birthday) AND
       R_who.id IS NOT NULL AND
       R_union1.A_bday IS NOT NULL
""").get
    assert(RDB2RDF(db2, sparqlSelect, StemURI("http://hr.example/DB/"), PrimaryKey(Attribute(Name("id"))), false) === sqlSelect)
  }

  test("transform optJoin1") {
    val sparqlParser = Sparql()
    val sparqlSelect = sparqlParser.parseAll(sparqlParser.select, """
PREFIX emplP: <http://hr.example/DB/Employee#>
PREFIX mangP: <http://hr.example/DB/Manage#>

SELECT ?empName ?managName ?grandManagName
 WHERE {          ?emp            emplP:lastName   ?empName
         OPTIONAL { ?mang         mangP:manages    ?emp .
                    ?mang         mangP:manager    ?manager .
                    ?manager      emplP:lastName   ?managName .
                    ?grandMang    mangP:manages    ?manager .
                    ?grandMang    mangP:manager    ?grandManager .
                    ?grandManager emplP:lastName   ?grandManagName } }
""").get
    val sqlParser = Sql()
    val sqlSelect = sqlParser.parseAll(sqlParser.select, """
SELECT R_emp.lastName AS A_empName, R_opt1.A_managName AS A_managName, R_opt1.A_grandManagName AS A_grandManagName
       FROM Employee AS R_emp
            LEFT OUTER JOIN (
    SELECT R_grandManager.lastName AS A_grandManagName, R_manager.lastName AS A_managName, R_mang.manages AS A_emp,
       R_grandMang.manager AS A_grandManager,
       R_mang.id AS A_mang,
       R_mang.manager AS A_manager,
       R_grandMang.id AS A_grandMang
           FROM Manage AS R_mang
                INNER JOIN Employee AS R_manager
                INNER JOIN Manage AS R_grandMang
                INNER JOIN Employee AS R_grandManager
     WHERE R_mang.manager=R_manager.id AND R_mang.manager=R_grandMang.manages AND R_grandMang.manager=R_grandManager.id
       AND (R_grandMang.manager IS NOT NULL)
       AND (R_mang.manages IS NOT NULL)
       AND (R_mang.id IS NOT NULL)
       AND (R_grandManager.lastName IS NOT NULL)
       AND (R_mang.manager IS NOT NULL)
       AND (R_manager.lastName IS NOT NULL)
       AND (R_grandMang.id IS NOT NULL)
             ) AS R_opt1 ON R_emp.id=R_opt1.A_emp
 WHERE R_emp.lastName IS NOT NULL
   AND R_emp.id IS NOT NULL
""").get
    assert(RDB2RDF(db2, sparqlSelect, StemURI("http://hr.example/DB/"), PrimaryKey(Attribute(Name("id"))), false) === sqlSelect)
  }

  test("transform nestOpt") {
    val sparqlParser = Sparql()
    val sparqlSelect = sparqlParser.parseAll(sparqlParser.select, """
PREFIX emplP: <http://hr.example/DB/Employee#>
PREFIX mangP: <http://hr.example/DB/Manage#>

SELECT ?empName ?managName ?grandManagName
 WHERE {          ?emp            emplP:lastName   ?empName
       OPTIONAL { ?mang           mangP:manages    ?emp .
                  ?mang           mangP:manager    ?manager .
                  ?manager        emplP:lastName   ?managName
         OPTIONAL { ?grandMang    mangP:manages    ?manager .
                    ?grandMang    mangP:manager    ?grandManager .
                    ?grandManager emplP:lastName   ?grandManagName } }
       }
""").get
    val sqlParser = Sql()
    val sqlSelect = sqlParser.parseAll(sqlParser.select, """
SELECT R_emp.lastName AS A_empName, R_opt1.A_managName AS A_managName, R_opt1.A_grandManagName AS A_grandManagName
       FROM Employee AS R_emp
            LEFT OUTER JOIN (
    SELECT R_opt2.A_grandManagName AS A_grandManagName, R_manager.lastName AS A_managName, R_mang.manages AS A_emp, R_mang.manager AS A_manager,
           R_mang.id AS A_mang, R_opt2.A_grandMang AS A_grandMang, R_opt2.A_grandManager AS A_grandManager
           FROM Manage AS R_mang
                INNER JOIN Employee AS R_manager
                LEFT OUTER JOIN (
        SELECT R_grandManager.lastName AS A_grandManagName, R_grandMang.manages AS A_manager,
               R_grandMang.id AS A_grandMang, R_grandMang.manager AS A_grandManager
               FROM Manage AS R_grandMang
                    INNER JOIN Employee AS R_grandManager
         WHERE R_grandMang.manager=R_grandManager.id AND R_grandMang.manages IS NOT NULL AND R_grandMang.manager IS NOT NULL
           AND R_grandManager.lastName IS NOT NULL AND R_grandMang.id IS NOT NULL
                 ) AS R_opt2 ON R_mang.manager=R_opt2.A_manager
          WHERE R_mang.manager=R_manager.id
            AND R_mang.manages IS NOT NULL AND R_mang.id IS NOT NULL AND R_mang.manager IS NOT NULL AND R_manager.lastName IS NOT NULL
             ) AS R_opt1 ON R_emp.id=R_opt1.A_emp
 WHERE R_emp.lastName IS NOT NULL AND R_emp.id IS NOT NULL
""").get
    assert(RDB2RDF(db2, sparqlSelect, StemURI("http://hr.example/DB/"), PrimaryKey(Attribute(Name("id"))), false) === sqlSelect)
  }
}
