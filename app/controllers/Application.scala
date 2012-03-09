package controllers

import play.api._
import play.api.mvc._

import scala.collection.JavaConversions.iterableAsScalaIterable
import org.neo4j.graphdb.DynamicRelationshipType
import org.neo4j.graphdb.GraphDatabaseService
import org.neo4j.rest.graphdb.RestGraphDatabase

import com.tinkerpop.blueprints.pgm.impls.neo4j.Neo4jGraph
import com.tinkerpop.gremlin.scala._

object Application extends Controller {
  val gds: GraphDatabaseService = new RestGraphDatabase(
    Option.apply(System.getenv("NEO4J_REST_URL")).getOrElse("http://localhost:7474/db/data"), System.getenv("NEO4J_LOGIN"), System.getenv("NEO4J_PASSWORD"))
  val g = new Neo4jGraph(gds)
  val me = gds.getReferenceNode()
  if (!me.hasProperty("name")) {
    me.setProperty("name", "I")
    val you = gds.createNode()
    you.setProperty("name", "you")
    me.createRelationshipTo(you, DynamicRelationshipType.withName("love"))
  }

  def index = Action {
//    val me = gds.getNodeById(0)
//    val rels = me.getRelationships(DynamicRelationshipType.withName("love"))
//    val rel = rels.head
//    val you = rel.getEndNode()
//    val message = me.getProperty("name") + " " + rel.getType().name() + " " + you.getProperty("name")
	val message = g.v(0).getProperty("name") + " " + g.v(0).outE.label.toIterable.head + " " + g.v(0).out.property("name").toIterable.head
    Ok(views.html.index(message))
  }
  
}
