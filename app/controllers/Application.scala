package controllers

import play.api._
import play.api.mvc._

import scala.collection.JavaConversions.iterableAsScalaIterable
import org.neo4j.graphdb.DynamicRelationshipType
import org.neo4j.graphdb.GraphDatabaseService
import org.neo4j.rest.graphdb.RestGraphDatabase

object Application extends Controller {
  val gds: GraphDatabaseService = new RestGraphDatabase(
    Option.apply(System.getenv("NEO4J_REST_URL")).getOrElse("http://localhost:7474/db/data"), System.getenv("NEO4J_LOGIN"), System.getenv("NEO4J_PASSWORD"))
  val me = gds.getReferenceNode()
  if (!me.hasProperty("name")) {
    me.setProperty("name", "I")
    val you = gds.createNode()
    you.setProperty("name", "you")
    me.createRelationshipTo(you, DynamicRelationshipType.withName("love"))
  }

  def index = Action {
    val me = gds.getNodeById(0)
    val rels = me.getRelationships(DynamicRelationshipType.withName("love"))
    val rel = rels.head
    val you = rel.getEndNode()
    val message = me.getProperty("name") + " " + rel.getType().name() + " " + you.getProperty("name")
    Ok(views.html.index(message))
  }
  
}
