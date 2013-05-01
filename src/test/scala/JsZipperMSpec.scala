package play.api.libs.json

import org.specs2.mutable._
import play.api.libs.json._
import play.api.libs.json.Json._
import play.api.libs.json.syntax._

import scala.util.control.Exception._
import java.text.ParseException

object JsZipperMSpec extends Specification {

  val js = Json.obj(
    "key1" -> Json.obj("key11" -> "value11", "key12" -> 123L, "key13" -> JsNull),
    "key2" -> 123,
    "key3" -> true,
    "key4" -> Json.arr("value41", 345.6, "test", Json.obj("key411" -> Json.obj("key4111" -> 987.654)))
  )


  "JsZipperM[Option]" should {
    "go through the whole tree" in {
      val zipper = JsZipperM[Option](js)

      zipper.value must beEqualTo(js)
      zipper.lefts must beEqualTo(Stream.Empty)
      zipper.rights must beEqualTo(Stream.Empty)
      zipper.parents must beEqualTo(Stream.Empty)
      zipper.parent must beEqualTo(Node.Empty)
      zipper.root.value must beEqualTo(js)
      
      val JsZipper(node, lefts, rights, parents) = zipper
      node must beEqualTo(Node(js))
      lefts must beEqualTo(Stream.Empty)
      rights must beEqualTo(Stream.Empty)
      parents must beEqualTo(Stream.Empty)

      // FIRST LINE
      val down10 = zipper.down
      down10.lefts must beEqualTo(Stream.Empty)
      down10.focus must beEqualTo(Node("key1", Json.obj("key11" -> "value11", "key12" -> 123L, "key13" -> JsNull)))
      down10.parent must beEqualTo(Node(js))
      down10.left must beEqualTo(JsZipper.Empty)
      
      val down11 = down10.right
      val JsZipper(node11, lefts11, rights11, parents11) = down11

      node11 must beEqualTo(Node("key2", JsNumber(123)))
      parents11 must beEqualTo(down10.parents)
      lefts11 must beEqualTo(down10.focus #:: down10.lefts)
      down10.rights must beEqualTo(down11.focus #:: rights11)

      val down12 = down11.right
      down12.focus must beEqualTo(Node("key3", JsBoolean(true)))
      down12.parents must beEqualTo(down10.parents)
      down12.lefts must beEqualTo(down11.focus #:: down11.lefts)
      rights11 must beEqualTo(down12.focus #:: down12.rights)

      val down13 = down12.right
      down13.focus must beEqualTo(Node("key4", Json.arr("value41", 345.6, "test", Json.obj("key411" -> Json.obj("key4111" -> 987.654)))))

      down13.right must beEqualTo(JsZipper.Empty)

      // 2ND LINE JsObject
      val down20 = down10.down
      down11.down must beEqualTo(JsZipper.Empty)
      down12.down must beEqualTo(JsZipper.Empty)

      down20.focus must beEqualTo(Node("key11", JsString("value11")))
      down20.lefts must beEqualTo(Stream.Empty)
      down20.parent must beEqualTo(down10.focus)

      val down21 = down20.right
      down21.focus must beEqualTo(Node("key12", JsNumber(123L)))
      down21.parents must beEqualTo(down21.parents)
      down21.lefts must beEqualTo(down20.focus #:: down20.lefts)
      down20.rights must beEqualTo(down21.focus #:: down21.rights)

      val down22 = down21.right
      down22.focus must beEqualTo(Node("key13", JsNull))
      // go left again
      val down21bis = down22.left
      down21bis.focus must beEqualTo(down21.focus)
      down21bis.lefts must beEqualTo(down21.lefts)
      down21bis.rights must beEqualTo(down21.rights)
      down21bis.parents must beEqualTo(down21.parents)

      down21.down must beEqualTo(JsZipper.Empty)

      // 2ND LINE JsArray
      val down23 = down13.down

      down23.focus must beEqualTo(Node(JsString("value41")))
      down23.lefts must beEqualTo(Stream.Empty)
      down23.parent must beEqualTo(down13.focus)

      down23.left must beEqualTo(JsZipper.Empty)
      down23.right.focus must beEqualTo(Node(JsNumber(345.6)))
      down23.right.right.focus must beEqualTo(Node(JsString("test")))
      down23.right.right.right.focus must beEqualTo(Node(Json.obj("key411" -> Json.obj("key4111" -> 987.654))))

      val rr = down23.right.right
      val rrrl = down23.right.right.right.left
      rr.focus must beEqualTo(rrrl.focus)
      rr.lefts must beEqualTo(rrrl.lefts)
      rr.rights must beEqualTo(rrrl.rights)
      rr.parents must beEqualTo(rrrl.parents)

      // 4th line JsObject again
      val down40 = down23.right.right.right.down.down
      down40.focus must beEqualTo(Node("key4111", JsNumber(987.654)))
      down40.down must beEqualTo(JsZipper.Empty)

      val down13bis = down23.up
      down13bis.focus must beEqualTo(down13.focus)
      down13bis.lefts must beEqualTo(down13.lefts)
      down13bis.rights must beEqualTo(down13.rights)
      down13bis.parents must beEqualTo(down13.parents)

      val zipperbis = down13.up
      zipperbis.focus must beEqualTo(zipper.focus)
      zipperbis.lefts must beEqualTo(zipper.lefts)
      zipperbis.rights must beEqualTo(zipper.rights)
      zipperbis.parents must beEqualTo(zipper.parents)

      zipperbis.up must beEqualTo(JsZipper.Empty)
    }

    "get path" in {
      val zipper = JsZipperM[Option](js)

      zipper.down.left.path must throwA[NoSuchElementException]
      zipper.down.left.pathSafe must beNone
      zipper.down.down.path must beEqualTo(__ \ "key1" \ "key11")
      zipper.down.down.right.path must beEqualTo(__ \ "key1" \ "key12")
      zipper.down.right.right.right.path must beEqualTo(__ \ "key4")
      zipper.down.right.right.right.down.path must beEqualTo((__ \ "key4")(0))
      zipper.down.right.right.right.down.right.path must beEqualTo((__ \ "key4")(1))
      zipper.down.right.right.right.down.right.right.path must beEqualTo((__ \ "key4")(2))
      zipper.down.right.right.right.down.left.pathSafe must beNone
    }

    "find path" in {
      val zipper = JsZipperM[Option](js)

      zipper.findPath(__ \ "key1" \ "key11").value must beEqualTo(JsString("value11"))
      zipper.findPath(__ \ "key2").value must beEqualTo(JsNumber(123))
      zipper.findPath((__ \ "key4")(1)).value must beEqualTo(JsNumber(345.6))
      zipper.findPath((__ \ "key4")(3) \ "key411" \ "key4111").value must beEqualTo(JsNumber(987.654))
      zipper.findPath(__ \ "key5") must beEqualTo(JsZipper.Empty)
      zipper.findPath((__ \ "key4")(4) \ "key411" \ "key4111") must beEqualTo(JsZipper.Empty)
      zipper.findPath(__).value must beEqualTo(zipper.value)
    }

    "update node" in {
      val zipper = JsZipperM[Option](js)
      
      val z = zipper.down.right.update(Some(JsString("toto")))

      z.map(_.root.value) must beEqualTo(Some(Json.obj(
        "key1" -> Json.obj("key11" -> "value11", "key12" -> 123L, "key13" -> JsNull),
        "key2" -> "toto",
        "key3" -> true,
        "key4" -> Json.arr("value41", 345.6, "test", Json.obj("key411" -> Json.obj("key4111" -> 987.654)))
      )))

    }

    "insertLeft node" in {
      val zipper = JsZipperM[Option](js)
      
      val z = zipper.down.right.right.right.down.right.insertValueLeft(Some(JsString("toto")))

      z.map(_.root.value) must beEqualTo(Some(Json.obj(
        "key1" -> Json.obj("key11" -> "value11", "key12" -> 123L, "key13" -> JsNull),
        "key2" -> 123,
        "key3" -> true,
        "key4" -> Json.arr("value41", "toto", 345.6, "test", Json.obj("key411" -> Json.obj("key4111" -> 987.654)))
      )))

    }

    "insertRight node" in {
      val zipper = JsZipperM[Option](js)
      
      val z = zipper.down.right.right.right.down.right.insertValueRight(Some(JsString("toto")))

      z.map(_.root.value) must beEqualTo(Some(Json.obj(
        "key1" -> Json.obj("key11" -> "value11", "key12" -> 123L, "key13" -> JsNull),
        "key2" -> 123,
        "key3" -> true,
        "key4" -> Json.arr("value41", 345.6, "toto", "test", Json.obj("key411" -> Json.obj("key4111" -> 987.654)))
      )))

    }

    "delete" in {
      val arr = Json.arr("toto", 123L, false)
      val zipper = JsZipperM[Option](arr)
      zipper.delete must beEqualTo(JsZipperM.Empty[Option]())
      zipper.down.delete.root.value must beEqualTo(Json.arr(123L, false))
      zipper.down.delete.delete.root.value must beEqualTo(Json.arr(false))
    }

    "find by value" in {

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

      val zipper = JsZipperM[Option](js)
      //zipper.findByValueLeftDown( _ == JsString("value11") ) must beEqualTo(JsZipper.Empty)
      zipper.bottomLeft.path must beEqualTo( __ \ 'key1 \ 'key11)
      zipper.bottomLeft.value must beEqualTo( JsString("TO_FIND") )

      zipper.bottomRight.path must beEqualTo( (__ \ 'key4)(3) \ 'key411 \ 'key4111 )
      zipper.bottomRight.value must beEqualTo( JsString("TO_FIND") )

      //println(zipper.down.right.right.right.down.rightStream.toList + "\n\n")

      //println("LFR:"+leaf.up.right.right.right.leftFocusRightStream.toList.mkString("\n") + "\n\n")
      //println(zipper.leftFocusRightDepthStream.toList.mkString("\n"))
      val zipper2 = zipper.findByValue(_ == JsString("TO_FIND"))
      zipper2.path must beEqualTo( __ \ 'key1 \ 'key11 )

      val zipper3 = zipper2.findNextByValue(_ == JsString("TO_FIND"))
      zipper3.path must beEqualTo( (__ \ 'key4)(0) )

      val zipper4 = zipper3.findNextByValue(_ == JsString("TO_FIND"))
      zipper4.path must beEqualTo( (__ \ 'key4)(3) \ 'key411 \ 'key4111 )
    }

    "find all" in {
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

      JsZipperM[Option](js).findAllByValue( _ == JsString("TO_FIND") ).map(_.pathValue).toList must beEqualTo(
        List(
          __ \ 'key1 \ 'key11 -> JsString("TO_FIND"),
          (__ \ 'key4)(0) -> JsString("TO_FIND"),
          (__ \ 'key4)(3) \ 'key411 \ 'key4111 -> JsString("TO_FIND")
        )
      )
      JsZipperM[Option](js).findAllByPathValue{ (path, value) => 
        path == (__ \ 'key4)(3) \ 'key411 \ 'key4111 && value == JsString("TO_FIND")
      }.map(_.pathValue).toList must beEqualTo(
        List(
          (__ \ 'key4)(3) \ 'key411 \ 'key4111 -> JsString("TO_FIND")
        )
      )
    }

    "find & update by value" in {
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

      val zipper = JsZipperM[Option](js)

      val res = 
        zipper.findByValue( _ == JsString("TO_FIND") )
              .update{ (js: JsValue) => 
                val JsString(str) = js; Some(JsString(str + "2")) 
              }.map(_.root)
      res.map(_.focus.value) must beEqualTo(Some(Json.obj(
        "key1" -> Json.obj(
          "key11" -> "TO_FIND2", 
          "key12" -> 123L, 
          "key13" -> JsNull
        ),
        "key2" -> 123,
        "key3" -> true,
        "key4" -> Json.arr("TO_FIND", 345.6, "test", Json.obj("key411" -> Json.obj("key4111" -> "TO_FIND")))
      )))
    }

    "multiple find update by value" in {
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

      var zipper = Option(JsZipperM[Option](js).findByValue( _ == JsString("TO_FIND") ))
      var res = zipper
      while(!zipper.get.isEmpty) {
        res = zipper flatMap (_.update{ (js:JsValue) => val JsString(str) = js; Some(JsString(str + "2")) })
        zipper = res map (_.findNextByValue( _ == JsString("TO_FIND") ))
      }
      println(res.map(_.root))
    }

    "update by path" in {
      JsZipperM[Option](js).createOrUpdate(
        __ \ "key1" \ "key11" -> Some(JsString("toto")),
        __ \ "key1" \ "key12" -> Some(JsNumber(234)),
        (__ \ "key4")(0)      -> Some(JsBoolean(true)),
        (__ \ "key4")(2)      -> Some(Json.arr(1, 2, 3))
      ).map(_.value) must beEqualTo(
        Some(Json.obj(
          "key1" -> Json.obj("key11" -> "toto", "key12" -> 234, "key13" -> JsNull),
          "key2" -> 123,
          "key3" -> true,
          "key4" -> Json.arr(true, 345.6, Json.arr(1, 2, 3), Json.obj("key411" -> Json.obj("key4111" -> 987.654)))
        ))
      )

    }

    "createOrUpdate path" in {
      val zipper = JsZipperM[Option](js)

      zipper.createOrUpdatePath(
        __ \ "key1" \ "key11",
        { (js:JsValue) => 
          val JsString(str) = js
          Some(JsString(str+ "_toto"))
        }
      ).map(_.value) must beEqualTo(Some(Json.obj(
        "key1" -> Json.obj("key11" -> "value11_toto", "key12" -> 123L, "key13" -> JsNull),
        "key2" -> 123,
        "key3" -> true,
        "key4" -> Json.arr("value41", 345.6, "test", Json.obj("key411" -> Json.obj("key4111" -> 987.654)))
      )))

      zipper.createOrUpdatePath(
        __ \ "key2",
        { (js:JsValue) => 
          val JsNumber(nb) = js
          Some(JsNumber(nb + 5))
        }
      ).map(_.value) must beEqualTo(Some(Json.obj(
        "key1" -> Json.obj("key11" -> "value11", "key12" -> 123L, "key13" -> JsNull),
        "key2" -> 128,
        "key3" -> true,
        "key4" -> Json.arr("value41", 345.6, "test", Json.obj("key411" -> Json.obj("key4111" -> 987.654)))
      )))
      
      zipper.createOrUpdatePath(
        (__ \ "key4")(1),
        { (js:JsValue) => 
          val JsNumber(nb) = js
          Some(JsNumber(nb + 3.0))
        }
      ).map(_.value) must beEqualTo(Some(Json.obj(
        "key1" -> Json.obj("key11" -> "value11", "key12" -> 123L, "key13" -> JsNull),
        "key2" -> 123,
        "key3" -> true,
        "key4" -> Json.arr("value41", 348.6, "test", Json.obj("key411" -> Json.obj("key4111" -> 987.654)))
      )))

      zipper.createOrUpdatePath(
        (__ \ "key4")(3) \ "key411" \ "key4111",
        { (js:JsValue) => 
          val JsNumber(nb) = js
          Some(JsNumber(nb + 3.0))
        }
      ).map(_.value) must beEqualTo(Some(Json.obj(
        "key1" -> Json.obj("key11" -> "value11", "key12" -> 123L, "key13" -> JsNull),
        "key2" -> 123,
        "key3" -> true,
        "key4" -> Json.arr("value41", 345.6, "test", Json.obj("key411" -> Json.obj("key4111" -> 990.654)))
      )))

      zipper.createOrUpdatePath(
        __,
        { (js:JsValue) => 
          Some(JsString("toto"))
        }
      ).map(_.value) must beEqualTo(Some(JsString("toto")))

      zipper.createOrUpdatePath(
        (__ \ "key4")(5),
        { (_:JsValue) => Some(JsBoolean(true)) }
      ).map(_.value) must beEqualTo(Some(Json.obj(
        "key1" -> Json.obj("key11" -> "value11", "key12" -> 123L, "key13" -> JsNull),
        "key2" -> 123,
        "key3" -> true,
        "key4" -> Json.arr("value41", 345.6, "test", Json.obj("key411" -> Json.obj("key4111" -> 987.654)), true)
      )))
    }

    "create from scratch" in {
      JsZipperM[Option](Json.obj())
        .createOrUpdatePath( 
          __ \ "key1" \ "key11", (_:JsValue) => Some(JsString("toto")) ).map(_.value) must beEqualTo(
          Some(Json.obj("key1" -> Json.obj("key11" -> "toto")))
        )

      JsZipperM[Option](Json.obj())
        .createOrUpdatePath( 
          (__ \ "key1")(0) \ "key11", (_:JsValue) => Some(JsString("toto")) ).map(_.value) must beEqualTo(
          Some(Json.obj("key1" -> Json.arr(Json.obj("key11" -> "toto"))))
        )

      JsZipperM[Option](Json.obj())
        .createOrUpdatePath( 
          (__ \ "key1")(1) \ "key11", (_:JsValue) => Some(JsNumber(123L)) ).map(_.value) must beEqualTo(
          Some(Json.obj("key1" -> Json.arr(Json.obj("key11" -> 123L))))
        )

      JsZipperM[Option](Json.arr())
        .createOrUpdatePath( __(0) \ "key1" \ "key11", (_:JsValue) => Some(JsNumber(123L)) ).map(_.value) must beEqualTo(
          Some(Json.arr(Json.obj("key1" -> Json.obj("key11" -> 123L))))
        )
    }

    "build" in {
      JsZipperM.buildJsObject[Option]( 
        __ \ "key1" \ "key11" -> Some(JsString("toto")),
        __ \ "key1" \ "key12" -> Some(JsNumber(123L)),
        (__ \ "key2")(0)      -> Some(JsBoolean(true)),
        __ \ "key3"           -> Some(Json.arr(1, 2, 3))
      ).map(_.value) must beEqualTo(
        Some(Json.obj(
          "key1" -> Json.obj(
            "key11" -> JsString("toto"),
            "key12" -> JsNumber(123L)
          ),
          "key2" -> Json.arr(true),
          "key3" -> Json.arr(1, 2, 3)
        )))

      JsZipperM.buildJsArray[Option](
         __(0) -> Some(JsNumber(123.45)),
         __(1) -> Some(JsString("toto"))
      ).map(_.value) must beEqualTo(
        Some(Json.arr(123.45, "toto"))
      )
    }
  }

  "JsZipperM[Future]" should {
    import scala.concurrent._
    import scala.concurrent.duration._
    import ExecutionContext.Implicits.global

    "update node" in {
      val zipper = JsZipperM[Future](js)
      
      val z = zipper.down.right.update(future{ JsString("toto") })

      Await.result(z.map(_.root.value), Duration("2 seconds")) must beEqualTo(Json.obj(
        "key1" -> Json.obj("key11" -> "value11", "key12" -> 123L, "key13" -> JsNull),
        "key2" -> "toto",
        "key3" -> true,
        "key4" -> Json.arr("value41", 345.6, "test", Json.obj("key411" -> Json.obj("key4111" -> 987.654)))
      ))
    }

    "update by path" in {
      Await.result(JsZipperM[Future](js).createOrUpdate(
        __ \ "key1" \ "key11" -> future{ JsString("toto") },
        __ \ "key1" \ "key12" -> future{ JsNumber(234) },
        (__ \ "key4")(0)      -> future{ JsBoolean(true) },
        (__ \ "key4")(2)      -> future{ Json.arr(1, 2, 3) }
      ).map(_.value), Duration("2 seconds")) must beEqualTo(
        Json.obj(
          "key1" -> Json.obj("key11" -> "toto", "key12" -> 234, "key13" -> JsNull),
          "key2" -> 123,
          "key3" -> true,
          "key4" -> Json.arr(true, 345.6, Json.arr(1, 2, 3), Json.obj("key411" -> Json.obj("key4111" -> 987.654)))
        )
      )
    }  

    "build" in {
      Await.result(JsZipperM.buildJsObject[Future]( 
        __ \ "key1" \ "key11" -> future{JsString("toto")},
        __ \ "key1" \ "key12" -> future{JsNumber(123L)},
        (__ \ "key2")(0)      -> future{JsBoolean(true)},
        __ \ "key3"           -> future{Json.arr(1, 2, 3)}
      ).map(_.value), Duration("2 seconds")) must beEqualTo(
        Json.obj(
          "key1" -> Json.obj(
            "key11" -> JsString("toto"),
            "key12" -> JsNumber(123L)
          ),
          "key2" -> Json.arr(true),
          "key3" -> Json.arr(1, 2, 3)
        ))

      Await.result(JsZipperM.buildJsArray[Future](
         __(0) -> future{JsNumber(123.45)},
         __(1) -> future{JsString("toto")}
      ).map(_.value), Duration("2 seconds")) must beEqualTo(
        Json.arr(123.45, "toto")
      )
    }  
  }
}

