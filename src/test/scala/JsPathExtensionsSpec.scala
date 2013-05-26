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

class JsPathExtensionsSpec extends Specification {

  "JsPathExtensions" should {
    "match key" in {
      val path = __ \ "toto"

      path match {
        case __ \ "toto" => success
        case __ \ key    => failure(s"expected tata but got $key")
      }

      path match {
        case __ \ "tata" => failure
        case __ \ key    => success
      }

    }

    "match 2keys" in {
      val path = __ \ "toto" \ "tata"

      path match {
        case __ \ "toto" \ "tata" => success
        case __ \ key \ key2    => failure(s"expected tata but got $key\\$key2")
      }

      path match {
        case __ \ "tata" \ "toto" => failure
        case _    => success
      }

      path match {
        case __ \ "toto" \ "toto" => failure
        case _    => success
      }

      val _ \ toto \ tata = __ \ "toto" \ "tata"
      toto must beEqualTo("toto")
      tata must beEqualTo("tata")

      val _ \ tata2 = __ \ "toto" \ "tata"
      tata2 must beEqualTo("tata")
    }

    "match 3keys" in {
      val path = __ \ "toto" \ "tata" \ "tutu"

      path match {
        case __ \ "toto" \ "tata" \ "tutu" => success
        case _  => failure
      }
      path match {
        case __ \ "tata" \ "tata" => failure
        case _    => success
      }

      val _ \ toto \ tata \ tutu = __ \ "toto" \ "tata" \ "tutu"

      toto must beEqualTo("toto")
      tata must beEqualTo("tata")
      tutu must beEqualTo("tutu")

    }

    "match 4keys" in {
      val _ \ toto \?\ tutu = __ \ "toto" \ "tata" \ "titi1" \ "titi2" \ "tutu"
      toto must beEqualTo("toto")
      tutu must beEqualTo("tutu")

      val _ \ tutu2 = __ \ "toto" \ "tata" \ "titi1" \ "titi2" \ "tutu"
      tutu2 must beEqualTo("tutu")

      val _ \ toto2 \?\ titi2 \ "tutu" = __ \ "toto" \ "tata" \ "titi1" \ "titi2" \ "tutu"
      toto2 must beEqualTo("toto")
      titi2 must beEqualTo("titi2")
    }

    "match 1idx" in {
      val toto@@2 = __(2)

      val _ @@ idx = __(2)
      idx must beEqualTo(2)

      val (_ \ toto2) @@ idx2 = (__ \ "toto")(3)
      idx2 must beEqualTo(3)
      toto2 must beEqualTo("toto")

      val (_ \ tata @@ idx3) \ tutu = (__ \ "tata")(2) \ "tutu"
      idx3 must beEqualTo(2)
      tata must beEqualTo("tata")
      tutu must beEqualTo("tutu")

      val (_ \ titi)@@idx4 \ _ \ titi2 = (__ \ "titi")(2) \ "blabla" \ "titi2"
      titi must beEqualTo("titi")
      idx4 must beEqualTo(2)
      titi2 must beEqualTo("titi2")

      val _@@idx5 \?\ titi3 = (__ \ "titi")(2) \ "blabla" \ "titi3"
      idx5 must beEqualTo(2)
      titi3 must beEqualTo("titi3")

      val (_ \ "titi")@@2 \?\ titi4 = (__ \ "titi")(2) \ "blabla" \ "titi4"

      import scala.util.matching.Regex

      val pattern = """al(\d)*pha""".r
      (__ \ "titi")(2) \ "al1234pha" \ "titi4" match {
        case (__ \ "titi")@@idx \ pattern(_) \ "titi4" => success
        case _ => failure
      }
    }
  }

}
