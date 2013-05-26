package controllers

import scala.concurrent._

import play.api._
import play.api.mvc._

import play.api.libs.functional._
import play.api.libs.iteratee._
import play.api.libs.ws._
import play.api.libs.oauth._

import play.api.libs.json._
import play.api.libs.json.syntax._
import play.api.libs.functional.syntax._
import play.api.libs.json.extensions._

import reactivemongo.api._
import play.modules.reactivemongo._
import play.modules.reactivemongo.json.collection.JSONCollection

import scala.concurrent.ExecutionContext.Implicits.global
import play.api.Play.current

object Application extends Controller with MongoController {

  def coll = db.collection[JSONCollection]("templates")


  def callWSFromTemplate(value: JsValue): Future[JsValue] = 
    WS.url((value \ "url").as[String])
      .withQueryString( "user_id" -> (value \ "user_id").as[String] )
      .get().map{ resp => resp.json }

  def dataSeq = Action{
    Async{
      for{
        templates <- coll.find(Json.obj()).cursor[JsObject].toList
        updated   <- Json.toJson(templates).updateAllM{
          case (_ \ "twitter", value) => callWSFromTemplate(value)
          case (_ \ "github", value)  => callWSFromTemplate(value)
          case (_, value)             => Future.successful(value)
        }
      } yield Ok(updated)
    }
  }

  def dataPar = Action{
    Async{
      coll.find(Json.obj()).cursor[JsObject].toList.flatMap{ templates =>
        // converts List[JsObject] into JsArray
        val jsonTemplates = Json.toJson(templates)

        // gathers all nodes that need to be updated
        val nodes = jsonTemplates.findAllM[Future]{
          case (_ \ "twitter", _) | (_ \ "github", _) => true
          case (_, value) => false
        }

        // launches WS calls in parallel and updates original JsArray
        Future.traverse(nodes){
          case (path@(_ \ "twitter"), value) => callWSFromTemplate(value).map( path -> _ )
          case (path@(_ \ "github"), value)  => callWSFromTemplate(value).map( path -> _ )
        }.map{ pathvalues => Ok(jsonTemplates.set(pathvalues:_*)) }
      }
    }
  }

  def provision = Action { Async {
    val values = Enumerator(
      Json.obj(
        "streams" -> Json.obj(
          "twitter" -> Json.obj(
            "url" -> "http://localhost:9000/twitter/statuses/user_timeline",
            "user_id" -> "twitter_nick1"
          ),
          "github" -> Json.obj(
            "url" -> "http://localhost:9000/github/users",
            "user_id" -> "github_nick1"
          )
        )
      ),

      Json.obj(
        "streams" -> Json.obj(
          "twitter" -> Json.obj(
            "url" -> "http://localhost:9000/twitter/statuses/user_timeline",
            "user_id" -> "twitter_nick2"
          ),
          "github" -> Json.obj(
            "url" -> "http://localhost:9000/github/users",
            "user_id" -> "github_nick2"
          )
        )
      ),

      Json.obj(
        "streams" -> Json.obj(
          "twitter" -> Json.obj(
            "url" -> "http://localhost:9000/twitter/statuses/user_timeline",
            "user_id" -> "twitter_nick3"
          ),
          "github" -> Json.obj(
            "url" -> "http://localhost:9000/github/users",
            "user_id" -> "github_nick3"
          )
        )
      )
    )

    coll.bulkInsert(values).map{ nb =>
      Ok(Json.obj("nb"->nb))
    }

  } }


}