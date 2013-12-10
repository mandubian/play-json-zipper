package play.api.libs.json

import org.specs2.mutable._

import org.junit.runner.RunWith
import org.specs2.runner.JUnitRunner
import org.specs2.specification.{Step, Fragments}

import play.api.libs.functional.syntax._
import play.api.libs.json._
import play.api.libs.json.extensions._
import play.api.libs.json.monad._
import play.api.libs.json.monad.syntax._

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

    "use json interpolation" in {
      val alpha = "foo"
      val beta = 123L
      val gamma = Json.arr(1, 2, 3)
      val delta = Json.obj("key1" -> "value1", "key2" -> "value2")
      val js = json"""
        {
          "alpha" : "$alpha",
          "beta" : $beta,
          "gamma" : $gamma,
          "delta" : $delta,
          "eta" : {
            "foo" : "bar",
            "foo2" : [ "bar21", 123, true, null ]
          }
        }
      """
      js must beEqualTo(
        Json.obj(
          "alpha" -> "foo",
          "beta" -> 123L,
          "gamma" -> Json.arr(1, 2, 3),
          "delta" -> Json.obj("key1" -> "value1", "key2" -> "value2"),
          "eta" -> Json.obj("foo" -> "bar", "foo2" -> Json.arr("bar21", 123L, true, JsNull))
        )
      )
    }

    val js2 = Json.obj(
        "key1" -> "value1",
        "key2" -> Json.arr(
          "alpha",
          Json.obj("foo" -> "bar", "foo2" -> Json.obj("key21" -> "value21", "key22" -> Json.arr("value221", 123L, false))),
          true,
          123.45
        )
      )

    "use json pattern matching 1" in {
      js2 match {
        case json"""{ "key1" : $v1, "key2" : ["alpha", $v2, true, $v3] }""" =>
          v1 must beEqualTo(JsString("value1"))
          v2 must beEqualTo(Json.obj("foo" -> "bar", "foo2" -> Json.obj("key21" -> "value21", "key22" -> Json.arr("value221", 123L, false))))
          v3 must beEqualTo(JsNumber(123.45))
          success
        case _ => failure
      }
    }

    "use json pattern matching 2" in {
      js2 match {
        case json"""{ "key1" : "value1", "key2" : ["alpha", { "foo" : "bar", "foo2" : { "key21" : $v1, "key22" : $v2 } }, true, 123.45] }""" =>
          v1 must beEqualTo(JsString("value21"))
          v2 must beEqualTo(Json.arr("value221", 123L, false))
          success
        case _ => failure
      }
    }

    "use json pattern matching 3" in {
      js2 match {
        case json"""{
          "key1" : "value2",
          "key2" : ["alpha", $v2, true, $v3]
        }"""   => failure
        case _ => success
      }
    }

    "use json pattern matching 4" in {
      Json.arr(1, 2, 3, 4) match {
        case json"[ $v1, 2, $v2, 4]" => 
          v1 must beEqualTo(JsNumber(1))
          v2 must beEqualTo(JsNumber(3))
          success
        case _ => failure
      }
    }

    "use json pattern matching 4bis" in {
      Json.arr(1, 2, 3, JsNull) match {
        case json"[ 1, 2, 3, $v1]" => 
          v1 must beEqualTo(JsNull)
          success
        case _ => failure
      }
    }

    "use json pattern matching 5" in {
      val json"[ $v1, 2, $v2, 4 ]" = Json.arr(1, 2, 3, 4)
      v1 must beEqualTo(JsNumber(1))
      v2 must beEqualTo(JsNumber(3))


      val json"""{ "key1" : $v3, "key2" : "value2", "key3" : $v4}""" = 
          json"""{ "key1" : 123.23, "key2" : "value2", "key3" : "value3"}"""
      v3 must beEqualTo(JsNumber(123.23))
      v4 must beEqualTo(JsString("value3"))

      case class FooBar(key1: String, key2: Long)
      json"""{ "key1" : 123, "key2" : "value2", "key3" : "value3"}""" match {
        case json"""{ "key1" : $v1, "key2" : "value2", "key3" : $v2 }""" => 
          FooBar(v2.as[String], v1.as[Long])
          success
        case _ => failure
      }
    }

  }

}
