package controllers

import play.api._
import play.api.mvc._

import play.api.libs.ws.WS
import play.api.libs.json._
import play.api.libs.concurrent.Promise

sealed trait Maybe[+T]
case class Error(json : JsValue) extends Maybe[Nothing]
case class Just[T](t : T) extends Maybe[T]

object Application extends Controller {
  val neo4jUrl = Option.apply(System.getenv("NEO4J_REST_URL")).getOrElse("http://localhost:7474/db/data")
  val neo4jLogin = System.getenv("NEO4J_LOGIN")
  val neo4jPassword = System.getenv("NEO4J_PASSWORD")
  
  def gremlin[T](script: String, params: JsObject = JsObject(Seq()))(implicit reads: Reads[T], manifest: Manifest[T]): Promise[Maybe[T]] = {
    WS.url(neo4jUrl + "/ext/GremlinPlugin/graphdb/execute_script").
    withAuth(neo4jLogin, neo4jPassword, com.ning.http.client.Realm.AuthScheme.BASIC).
    post(JsObject(Seq(
        "script" -> JsString(script),
        "params" -> params
    ))) map { response => response.json.asOpt[T] match {
      case None => Error(response.json)
      case Some(t) => Just(t)
    }}
  }

  def getRecommendations(id: Int) = {
    gremlin[Array[String]]("""
        Gremlin.defineStep('corated',[Vertex,Pipe], { def stars ->
          _().inE('rated').filter{it.getProperty('stars') > stars}.outV.outE('rated').filter{it.getProperty('stars') > stars}.inV})
        m = [:];
        g.v(id).corated(3).title.groupCount(m).iterate();
        m.sort{a,b -> b.value <=> a.value}[0..9].keySet()
    """, JsObject(Seq("id" -> JsNumber(id))))
  }
  
  def index = Action {
    AsyncResult { getRecommendations(1) map { result =>
      Ok(views.html.index(result))
    }}
  }
  
}
