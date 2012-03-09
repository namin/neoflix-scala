import sbt._
import Keys._
import PlayProject._

object ApplicationBuild extends Build {

    val appName         = "neosample"
    val appVersion      = "1.0-SNAPSHOT"

    val appDependencies = Seq(
      "org.neo4j" % "neo4j-rest-graphdb" % "1.6",
      "com.tinkerpop.gremlin" % "gremlin-scala" % "1.5",
      "com.tinkerpop.blueprints" % "blueprints-neo4j-graph" % "1.2"
    )

    val main = PlayProject(appName, appVersion, appDependencies, mainLang = SCALA).settings(
      resolvers += "Neo4j Release Repository" at "http://m2.neo4j.org/content/repositories/releases"
    )

}
