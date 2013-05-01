package play.api.libs.json

import org.specs2.mutable._

import org.junit.runner.RunWith
import org.specs2.runner.JUnitRunner
import org.specs2.specification.{Step, Fragments}

import play.api.libs.json._
import syntax._
import play.api.libs.functional.syntax._
import play.api.libs.json.extensions._

import scala.concurrent._
import scala.concurrent.duration._
import ExecutionContext.Implicits.global

class JsExtensionsSpec extends Specification {
  val js = Json.obj(
    "key1" -> Json.obj(
      "key11" -> "TO_FIND", 
      "key12" -> 123L, 
      "key13" -> JsNull
    ),
    "key2" -> 123,
    "key3" -> true,
    "key4" -> Json.arr("TO_FIND", 345.6, "test", Json.obj("key411" -> Json.obj("key4111" -> "TO_FIND")))
  )

  "JsExtensions" should {
    "set" in {
      js.set( 
        (__ \ "key4")(2) -> JsNumber(765.23),
        (__ \ "key1" \ "key12") -> JsString("toto") 
      ) must beEqualTo(
        Json.obj(
          "key1" -> Json.obj(
            "key11" -> "TO_FIND", 
            "key12" -> "toto", 
            "key13" -> JsNull
          ),
          "key2" -> 123,
          "key3" -> true,
          "key4" -> Json.arr("TO_FIND", 345.6, 765.23, Json.obj("key411" -> Json.obj("key4111" -> "TO_FIND")))
        )
      )
    }

    "delete" in {
      js.delete( 
        (__ \ "key4")(2)
      ) must beEqualTo(
        Json.obj(
          "key1" -> Json.obj(
            "key11" -> "TO_FIND", 
            "key12" -> 123L, 
            "key13" -> JsNull
          ),
          "key2" -> 123,
          "key3" -> true,
          "key4" -> Json.arr("TO_FIND", 345.6, Json.obj("key411" -> Json.obj("key4111" -> "TO_FIND")))
        )
      )
    }

    "delete" in {
      js.delete( 
        (__ \ "key4")(2),
        (__ \ "key1" \ "key12"),
        (__ \ "key1" \ "key13")
      ) must beEqualTo(
        Json.obj(
          "key1" -> Json.obj(
            "key11" -> "TO_FIND"
          ),
          "key2" -> 123,
          "key3" -> true,
          "key4" -> Json.arr("TO_FIND", 345.6, Json.obj("key411" -> Json.obj("key4111" -> "TO_FIND")))
        )
      )
    }

    "find all" in {
      js.findAll( _ == JsString("TO_FIND") ).toList must beEqualTo(
        List(
          (__ \ 'key1 \ 'key11, JsString("TO_FIND")), 
          ((__ \ 'key4)(0), JsString("TO_FIND")), 
          ((__ \ 'key4)(3) \ 'key411 \ 'key4111, JsString("TO_FIND"))
        )
      )
      success
    }

    "update all by value" in {
      js.updateAll( (_:JsValue) == JsString("TO_FIND") ){ js =>
        val JsString(str) = js
        JsString(str + "2")
      } must beEqualTo(
        Json.obj(
          "key1" -> Json.obj(
            "key11" -> "TO_FIND2", 
            "key12" -> 123L, 
            "key13" -> JsNull
          ),
          "key2" -> 123,
          "key3" -> true,
          "key4" -> Json.arr("TO_FIND2", 345.6, "test", Json.obj("key411" -> Json.obj("key4111" -> "TO_FIND2")))
        )
      )
      success
    }

    "update all by pathvalue" in {
      js.updateAll{ (path, js) => 
        JsPathExtension.hasKey(path) == Some("key4111") 
      }{ (path, js) =>
        val JsString(str) = js
        JsString(str + path.path.last)
      } must beEqualTo(
        Json.obj(
          "key1" -> Json.obj(
            "key11" -> "TO_FIND", 
            "key12" -> 123L, 
            "key13" -> JsNull
          ),
          "key2" -> 123,
          "key3" -> true,
          "key4" -> Json.arr("TO_FIND", 345.6, "test", Json.obj("key411" -> Json.obj("key4111" -> "TO_FIND/key4111")))
        )
      )
      success
    }

    "setM" in {
      Await.result(
        js.setM[Future]( 
          (__ \ "key4")(2)        -> future{ JsNumber(765.23) },
          (__ \ "key1" \ "key12") -> future{ JsString("toto") }
        ),
        Duration("2 seconds")
      ) must beEqualTo(
        Json.obj(
          "key1" -> Json.obj(
            "key11" -> "TO_FIND", 
            "key12" -> "toto", 
            "key13" -> JsNull
          ),
          "key2" -> 123,
          "key3" -> true,
          "key4" -> Json.arr("TO_FIND", 345.6, 765.23, Json.obj("key411" -> Json.obj("key4111" -> "TO_FIND")))
        )
      )
    }

    "updateM all by value" in {
      Await.result(
        js.updateAllM[Future]( (_:JsValue) == JsString("TO_FIND") ){ js =>
          future {
            val JsString(str) = js
            JsString(str + "2")
          }
        },
        Duration("2 seconds")
      ) must beEqualTo(
        Json.obj(
          "key1" -> Json.obj(
            "key11" -> "TO_FIND2", 
            "key12" -> 123L, 
            "key13" -> JsNull
          ),
          "key2" -> 123,
          "key3" -> true,
          "key4" -> Json.arr("TO_FIND2", 345.6, "test", Json.obj("key411" -> Json.obj("key4111" -> "TO_FIND2")))
        )
      )
      success
    }
  }

}
