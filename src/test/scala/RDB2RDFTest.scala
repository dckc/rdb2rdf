package w3c.sw

import org.scalatest.FunSuite
import java.net.URI

class RDB2RDFTest extends FunSuite {

  test("SQLbgp") {
    val sparqlParser = Sparql()
    val sparqlSelect = sparqlParser.parseAll(sparqlParser.select, """
SELECT ?empName ?manageName {
?emp      <http://hr.example/DB/Employee#lastName>   ?empName .
?emp      <http://hr.example/DB/Employee#manager>    ?manager .
?manager  <http://hr.example/DB/Employee#lastName>   ?managName
}
""").get
    val sqlParser = Sql()
    val sqlSelect = sqlParser.parseAll(sqlParser.select, """
SELECT emp.lastName AS empName, manager.lastName AS managName
       FROM Employee AS emp
            INNER JOIN Employee AS manager ON manager.id=emp.manager
 WHERE emp.lastName IS NOT NULL AND manager.lastName IS NOT NULL
""").get
    // assert(RDB2RDF(sparqlSelect, StemURI("http://hr.example/DB/"), PrimaryKey(Attribute(Name("id")))) === sqlSelect)
    true
  }


}
