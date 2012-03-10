package controllers

import play.api._
import play.api.mvc._
import play.api.libs.ws.WS
import play.api.libs.json._
import play.api.libs.concurrent.Promise
import org.slf4j.LoggerFactory
import java.net.URLEncoder
import play.api.libs.concurrent.Akka
import play.api.Play.current
import play.api.templates.Html

case class Movie(url: String, posterUrl: String, tagline: String, rating: Double, overview: String)

object Application extends Controller {
  val logger = LoggerFactory.getLogger("controllers.Application");

  val neo4jUrl = Option.apply(System.getenv("NEO4J_REST_URL")).getOrElse("http://localhost:7474/db/data")
  val neo4jLogin = System.getenv("NEO4J_LOGIN")
  val neo4jPassword = System.getenv("NEO4J_PASSWORD")

  val tmdbUrl = "http://api.themoviedb.org/3"
  val tmdbKey = System.getenv("TMDB_KEY")

  def gremlin(script: String, params: JsObject = JsObject(Seq())) = {
    WS.url(neo4jUrl + "/ext/GremlinPlugin/graphdb/execute_script").
    withAuth(neo4jLogin, neo4jPassword, com.ning.http.client.Realm.AuthScheme.BASIC).
    post(JsObject(Seq(
        "script" -> JsString(script),
        "params" -> params
    ))) map { response => response.json }
  }

 implicit object MovieFormat extends Reads[Movie] {
    def reads(json: JsValue): Movie = Movie(
      url = "http://www.themoviedb.org/movie/" + (json \ "id").as[Int],
      posterUrl = "http://cf2.imgobject.com/t/p/w92/" + (json \ "poster_path").as[String],
      tagline = (json \ "tagline").as[String],
      rating = (json \ "vote_average").as[Double],
      overview = (json \ "overview").as[String]
    )
  }
  
  def tmdb(title: String) = {
    println(tmdbUrl + "/search/movie?api_key=" + tmdbKey + "&query=" + URLEncoder.encode(title, "UTF-8"))
    WS.url(tmdbUrl + "/search/movie?api_key=" + tmdbKey + "&query=" + URLEncoder.encode(title, "UTF-8")).get flatMap { response =>
      ((response.json \ "results")(0) \ "id").asOpt[Int] match { 
        case None => Akka.future(None)
        case Some(id) => 
          println(tmdbUrl + "/movie/" + id + "?api_key=" + tmdbKey)
          WS.url(tmdbUrl + "/movie/" + id + "?api_key=" + tmdbKey).get map { _.json.asOpt[Movie] }
      }
    }
  }

  def get_recommendations(id: Int) = {
    gremlin("""
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
      JsObject(Seq(
          "id" -> JsNumber(id),
          "name" -> JsString(if (recs.toString == "[]") "No Recommendations" else "Recommendations"),
          "value" -> JsObject(recs.as[Array[String]].flatMap({v: String =>
            val (a, b) = v.splitAt(v.indexOf(":"))
            Seq("id" -> JsNumber(a.toInt), "name" -> JsString(b.drop(1)))
      }))))})
  }
    
  def recommendations(id: Int) = Action { AsyncResult {
    get_recommendations(id).map(Ok(_))
  }}

  def get_poster(title: String): Promise[Html] =
    tmdb(title) map { maybeMovie => views.html.poster(maybeMovie) }
  
  def poster = Action { request => AsyncResult {
    println(request.queryString("title").head)
    get_poster(request.queryString("title").head).map(Ok(_))
  }}
  
  def index = Action {
    Ok(views.html.index())
  }
  
}
