package controllers

import play.api._
import play.api.mvc._

import com.typesafe.config._

object Application extends Controller {
  def index = Action {
    val conf = ConfigFactory.load()
    Ok(views.html.index(conf.getString("neo4j.url")))
  }
  
}
