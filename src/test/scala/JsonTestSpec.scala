import org.specs2.mutable._

import org.junit.runner.RunWith
import org.specs2.runner.JUnitRunner
import org.specs2.specification.{Step, Fragments}

import play.api.libs.json._
import play.api.libs.functional._

class JsonTestSpec extends Specification {
  case class EucalyptusTree(col:Int, row: Int)

  object EucalyptusTree{
    implicit val fmt = Json.format[EucalyptusTree]
  }

  case class Koala(name: String, home: EucalyptusTree)

  object Koala{
    implicit val fmt = Json.format[Koala]
  }

  "Json" should {
    "work standalone" in {
      val kaylee = Koala("kaylee", EucalyptusTree(10, 23))

      println(Json.prettyPrint(Json.toJson(kaylee)))

      Json.fromJson[Koala](
        Json.obj(
          "name" -> "kaylee", 
          "home" -> Json.obj(
            "col" -> 10, 
            "row" -> 23
          )
        )
      ).get must beEqualTo(kaylee)
      success
    }
  }

}
