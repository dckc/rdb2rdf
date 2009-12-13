import sbt._

class RDB2RDF(info: ProjectInfo) extends DefaultProject(info) {

  val scalatools = "scala-tools" at "http://scala-tools.org/repo-snapshots"

  val scalatest = "org.scalatest" % "scalatest" % "1.0.1-for-scala-2.8.0.Beta1-RC1-with-test-interfaces-0.2-SNAPSHOT" % "test->default"

}
