package controllers

import play.api._
import play.api.mvc._
import play.api.libs.ws.WS
import play.api.libs.json._
import play.api.libs.concurrent.Promise
import org.slf4j.LoggerFactory

object Application extends Controller {
  val logger = LoggerFactory.getLogger("controllers.Application");
  val neo4jUrl = Option.apply(System.getenv("NEO4J_REST_URL")).getOrElse("http://localhost:7474/db/data")
  val neo4jLogin = System.getenv("NEO4J_LOGIN")
  val neo4jPassword = System.getenv("NEO4J_PASSWORD")
  
  def gremlin(script: String, params: JsObject = JsObject(Seq())) = {
    WS.url(neo4jUrl + "/ext/GremlinPlugin/graphdb/execute_script").
    withAuth(neo4jLogin, neo4jPassword, com.ning.http.client.Realm.AuthScheme.BASIC).
    post(JsObject(Seq(
        "script" -> JsString(script),
        "params" -> params
    ))) map { response => response.json }
  }

  def recommendations(id: Int) = Action {
    AsyncResult { gremlin("""
        m = [:];
    	v = g.v(node_id);

    	v.
    	inE('rated').
    	filter{it.getProperty('stars') > 3}.
    	outV.
    	outE('rated').
    	filter{it.getProperty('stars') > 3}.
    	inV.
    	filter{it != v}.
    	groupCount(m){"${it.id}:${it.title}"}.iterate();

        m.sort{a,b -> b.value <=> a.value}[0..9].keySet();
    """, JsObject(Seq("node_id" -> JsNumber(id)))).map({ recs =>
      Ok(JsObject(Seq(
          "id" -> JsNumber(id),
          "name" -> JsString(if (recs.toString == "[]") "No Recommendations" else "Recommendations"),
          "value" -> JsObject(recs.as[Array[String]].flatMap({v: String =>
            val (a, b) = v.splitAt(v.indexOf(":"))
            Seq("id" -> JsNumber(a.toInt), "name" -> JsString(b.drop(1)))
          })))))
    })}
  }
  
  def index = Action {
    Ok(views.html.index())
  }
  
}
