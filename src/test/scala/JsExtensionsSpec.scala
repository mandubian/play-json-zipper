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
      js.findAllByValue( _ == JsString("TO_FIND") ).toList must beEqualTo(
        List(
          (__ \ 'key1 \ 'key11, JsString("TO_FIND")),
          ((__ \ 'key4)(0), JsString("TO_FIND")),
          ((__ \ 'key4)(3) \ 'key411 \ 'key4111, JsString("TO_FIND"))
        )
      )
      success
    }

    "update all by value" in {
      js.filterUpdateAllByValue( (_:JsValue) == JsString("TO_FIND") ){ js =>
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
      js.filterUpdateAll{ (path, js) =>
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
        js.filterUpdateAllByValueM[Future]( (_:JsValue) == JsString("TO_FIND") ){ js =>
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

    "updateM all by path/value" in {
      Await.result(
        js.updateAllM[Future]{
          case ( __ \ "key1" \ "key11",                   JsString(str) ) => future { JsString(str + "2"): JsValue }
          case ( (__ \ "key4")@@0,                        JsString(str) ) => future { JsString(str + "2"): JsValue }
          case ( (__ \ "key4")@@3 \ "key411" \ "key4111", JsString(str) ) => future { JsString(str + "2"): JsValue }
          case (path, value) => Future.successful(value)
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

    "update one path" in {
      js.update(
        (__ \ "key4")(3) \ "key411",
        { js => val JsString(str) = js \ "key4111"; JsString(str+"123") }
      ) must beEqualTo(
        Json.obj(
          "key1" -> Json.obj(
            "key11" -> "TO_FIND",
            "key12" -> 123L,
            "key13" -> JsNull
          ),
          "key2" -> 123,
          "key3" -> true,
          "key4" -> Json.arr("TO_FIND", 345.6, "test", Json.obj("key411" -> "TO_FIND123"))
        )
      )
    }


    "update path not found" in {
      js.update(
        (__ \ "key5"),
        { js => val JsString(str) = js \ "key4111"; JsString(str+"123") }
      ) must beEqualTo(js)
    }

    "update all keynodes" in {
      val obj = Json.obj(
        "_id" -> Json.obj("$oid" -> "1234"),
        "key1" -> Json.obj(
          "_id" -> Json.obj("$oid" -> "9876")
        ),
        "key2" -> Json.arr(
          "alpha",
          123,
          Json.obj(
            "_id" -> Json.obj("$oid" -> "4567")
          )
        )
      )
      obj.updateAllKeyNodes{
        case ((_ \ "_id"), value) => ("id" -> value \ "$oid")
      } must beEqualTo(
        Json.obj(
          "id" -> "1234",
          "key1" -> Json.obj(
            "id" -> "9876"
          ),
          "key2" -> Json.arr(
            "alpha",
            123,
            Json.obj(
              "id" -> "4567"
            )
          )
        )
      )
    }
  }

}
