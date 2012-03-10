package controllers

import play.api._
import play.api.mvc._

import play.api.libs.ws.WS
import play.api.libs.json._
import play.api.libs.concurrent.Promise
import play.api.libs.concurrent.Akka
import play.api.templates.Html
import play.api.Play.current

import java.net.URLEncoder
import java.net.URLDecoder

case class Movie(url: String, posterUrl: String, tagline: String, rating: Double, overview: String)

object Application extends Controller {
  val neo4jUrl = Option.apply(System.getenv("NEO4J_URL")).getOrElse("http://localhost:7474")
  val neo4jLogin = System.getenv("NEO4J_LOGIN")
  val neo4jPassword = System.getenv("NEO4J_PASSWORD")

  val tmdbUrl = "http://api.themoviedb.org/3"
  val tmdbKey = System.getenv("TMDB_KEY")

  def gremlin(script: String, params: JsObject = JsObject(Seq())) = {
    WS.url(neo4jUrl + "/db/data/ext/GremlinPlugin/graphdb/execute_script").
    withAuth(neo4jLogin, neo4jPassword, com.ning.http.client.Realm.AuthScheme.BASIC).
    post(JsObject(Seq(
        "script" -> JsString(script),
        "params" -> params
    ))) map { _.json }
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
    WS.url(tmdbUrl + "/search/movie?api_key=" + tmdbKey + "&query=" + URLEncoder.encode(title, "UTF-8")).get flatMap { response =>
      ((response.json \ "results")(0) \ "id").asOpt[Int] match { 
        case None => Akka.future(None)
        case Some(id) => 
          WS.url(tmdbUrl + "/movie/" + id + "?api_key=" + tmdbKey).get map { _.json.asOpt[Movie] }
      }
    }
  }

  def gremlinExec(script: String) = {
    gremlin(script).value.get
  }
  
  def gremlinAsync(msg: String, script: String) {
    gremlin(script).onRedeem(_ => ())
    println(msg)
  }
  
  def create_graph(): Unit = {
    println("Starting...")

    if (gremlinExec("g.idx('vertices')[[type:'Movie']].count()").asOpt[Int].getOrElse(0) > 0) {
      println("Graph already exists.")
      return
    }

    if (gremlinExec("g.indices").as[Array[String]].isEmpty) {
      gremlin("g.createAutomaticIndex('vertices', Vertex.class, null);") 
      if (gremlinExec("g.V.count()").asOpt[Int].getOrElse(0) > 0)
        gremlinExec("AutomaticIndexHelper.reIndexElements(g, g.idx('vertices'), g.V);")
    }

    gremlinAsync("Creating the graph is going to take some time, watch it on " + neo4jUrl, """
            g.setMaxBufferSize(1000);

            'http://neoflix.heroku.com/movies.dat'.toURL().eachLine { def line ->
              def components = line.split('::');
              def movieVertex = g.addVertex(['type':'Movie', 'movieId':components[0].toInteger(), 'title':components[1]]);
              components[2].split('\\\\|').each { def genera ->
                def hits = g.idx(Tokens.T.v)[[genera:genera]].iterator();
                def generaVertex = hits.hasNext() ? hits.next() : g.addVertex(['type':'Genera', 'genera':genera]);
                g.addEdge(movieVertex, generaVertex, 'hasGenera');
              }
            };

            occupations = [0:'other', 1:'academic/educator', 2:'artist',
              3:'clerical/admin', 4:'college/grad student', 5:'customer service',
              6:'doctor/health care', 7:'executive/managerial', 8:'farmer',
              9:'homemaker', 10:'K-12 student', 11:'lawyer', 12:'programmer',
              13:'retired', 14:'sales/marketing', 15:'scientist', 16:'self-employed',
              17:'technician/engineer', 18:'tradesman/craftsman', 19:'unemployed', 20:'writer'];

            'http://neoflix.heroku.com/users.dat'.toURL().eachLine { def line ->
              def components = line.split('::');
              def userVertex = g.addVertex(['type':'User', 'userId':components[0].toInteger(), 'gender':components[1], 'age':components[2].toInteger()]);
              def occupation = occupations[components[3].toInteger()];
              def hits = g.idx(Tokens.T.v)[[occupation:occupation]].iterator();
              def occupationVertex = hits.hasNext() ? hits.next() : g.addVertex(['type':'Occupation', 'occupation':occupation]);
              g.addEdge(userVertex, occupationVertex, 'hasOccupation');
            };

            'http://neoflix.heroku.com/ratings.dat'.toURL().eachLine {def line ->
              def components = line.split('::');
              def ratedEdge = g.addEdge(g.idx(Tokens.T.v)[[userId:components[0].toInteger()]].next(), g.idx(T.v)[[movieId:components[1].toInteger()]].next(), 'rated');
              ratedEdge.setProperty('stars', components[2].toInteger());
             };

             g.stopTransaction(TransactionalGraph.Conclusion.SUCCESS);""")
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
        groupCount(m){"${it.id}:${it.title.replaceAll(',',' ')}"}.iterate();
        
        m.sort{a,b -> b.value <=> a.value}[0..14];
    """, JsObject(Seq("node_id" -> JsInteger(id)))).map({ r =>
      val recs = r.as[String] // we don't get a JSON object back for a Groovy map :-(
      JsArray(Array(JsObject(Seq(
          "id" -> JsInteger(id),
          "name" -> JsString(if (recs == "{}") "No Recommendations" else "Recommendations"),
          "values" -> JsArray(if (recs == "{}") Array(JsObject(Seq("id" -> JsInteger(id), "name" -> JsString("No Recommendations"))))
              else recs.drop(1).dropRight(1).split(",").map({v: String =>
            	val (a, b) = v.splitAt(v.indexOf(":"))
            	JsObject(Seq("id" -> JsInteger(a.trim.toInt), "name" -> JsString(b.drop(1))))
      }))))))})
  }
    
  def recommendations(id: Int) = Action { AsyncResult {
    get_recommendations(id).map(Ok(_))
  }}

  def get_poster(title: String): Promise[Html] =
    tmdb(title) map { maybeMovie => views.html.poster(maybeMovie) }
  
  def poster = Action { request => AsyncResult {
    get_poster(request.queryString("title").head).map(Ok(_))
  }}
  
  def get_title(id: Int) = {
    gremlin(
        "g.v(node_id).title",
        JsObject(Seq("node_id" -> JsInteger(id)))).map(_.as[String])
  }
  
  def title(id: Int) = Action { AsyncResult {
    get_title(id).map(Ok(_))
  }}

  def get_id_from_title(title: String) = {
    gremlin(
        "g.idx(Tokens.T.v)[[title:title]].next().id",
        JsObject(Seq("title" -> JsString(title)))).map(_.as[Int])
  }
  
  def id_from_title = Action { request => AsyncResult {
    get_id_from_title(request.queryString("title").head).map({id:Int => Ok(id.toString())})
  }}
  
  def resources_show = Action { request =>
    val pid = request.queryString("id").head
    val (id: Int, title: String) = try {
      val id = pid.toInt
      (id, get_title(id).value.get)
    } catch {
      case e : NumberFormatException =>
        val title = URLDecoder.decode(pid, "UTF-8")
        (get_id_from_title(title).value.get, title)
    }
    
    AsyncResult { get_poster(title).map{poster => 
      AsyncResult { get_recommendations(id).map{recs =>
    	Ok(JsObject(Seq(
    			"details_html" -> JsString(views.html.fullposter(title, poster).body),
    			"data" -> JsObject(Seq(
    			    "attributes" -> recs,
    			    "name" -> JsString(title),
    			    "id" -> JsInteger(id)))))).
        withHeaders(("Cache-Control", "public, max-age=2592000"))
    }}}}
  }
  
  def index = Action { request =>
    Ok(views.html.index(neo4jUrl))
  }
  
}
