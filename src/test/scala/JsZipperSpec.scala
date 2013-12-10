package play.api.libs.json

import org.specs2.mutable._
import play.api.libs.json._
import play.api.libs.json.Json._

import scala.util.control.Exception._
import java.text.ParseException

object JsZipperSpec extends Specification {

  val js = Json.obj(
    "key1" -> Json.obj("key11" -> "value11", "key12" -> 123L, "key13" -> JsNull),
    "key2" -> 123,
    "key3" -> true,
    "key4" -> Json.arr("value41", 345.6, "test", Json.obj("key411" -> Json.obj("key4111" -> 987.654)))
  )


  "JsZipper" should {
    "go through the whole tree" in {
      val zipper = JsZipper(js)

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
      val zipper = JsZipper(js)

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
      val zipper = JsZipper(js)

      zipper.findPath(__ \ "key1" \ "key11").value must beEqualTo(JsString("value11"))
      zipper.findPath(__ \ "key2").value must beEqualTo(JsNumber(123))
      zipper.findPath((__ \ "key4")(1)).value must beEqualTo(JsNumber(345.6))
      zipper.findPath((__ \ "key4")(3) \ "key411" \ "key4111").value must beEqualTo(JsNumber(987.654))
      zipper.findPath(__ \ "key5") must beEqualTo(JsZipper.Empty)
      zipper.findPath((__ \ "key4")(4) \ "key411" \ "key4111") must beEqualTo(JsZipper.Empty)
      zipper.findPath(__).value must beEqualTo(zipper.value)
    }

    "update" in {
      val zipper = JsZipper(js)
      val d11 = zipper.down.right
      val d11bis = d11.update(JsString("toto"))
      d11bis.focus must beEqualTo(KeyNode("key2", JsString("toto")))

      val jsAfter = Json.obj(
        "key1" -> Json.obj("key11" -> "value11", "key12" -> 123L, "key13" -> JsNull),
        "key2" -> "toto",
        "key3" -> true,
        "key4" -> Json.arr("value41", 345.6, "test", Json.obj("key411" -> Json.obj("key4111" -> 987.654)))
      )
      d11bis.up.focus must beEqualTo(Node(jsAfter))

      val d130 = zipper.down.right.right.right.down
      val d130bis = d130.update(JsNumber(123L))
      val jsAfter2 = Json.obj(
        "key1" -> Json.obj("key11" -> "value11", "key12" -> 123L, "key13" -> JsNull),
        "key2" -> 123,
        "key3" -> true,
        "key4" -> Json.arr(123L, 345.6, "test", Json.obj("key411" -> Json.obj("key4111" -> 987.654)))
      )
      d130bis.root.focus must beEqualTo(Node(jsAfter2))

      zipper.down.right.update(Json.arr("tata", 234.45)).
            right.right.down.right.right.right.down
            .update(JsBoolean(false)).root.focus must beEqualTo(
        Node(Json.obj(
          "key1" -> Json.obj("key11" -> "value11", "key12" -> 123L, "key13" -> JsNull),
          "key2" -> Json.arr("tata", 234.45),
          "key3" -> true,
          "key4" -> Json.arr("value41", 345.6, "test", Json.obj("key411" -> false))
        ))
      )

      val jsAfter3 = Json.obj(
        "key1" -> Json.obj("key11" -> "value11", "key12" -> 123L, "key13" -> JsNull),
        "key2" -> "133",
        "key3" -> true,
        "key4" -> Json.arr("value41", 345.6, "test", Json.obj("key411" -> Json.obj("key4111" -> 987.654)))
      )

      d11.update( js => js match {
        case JsNumber(nb) => JsString((nb + 10).toString)
        case js => js
      }).root.value must beEqualTo(jsAfter3)

      val jsAfter4 = Json.obj(
        "key1" -> Json.obj("key11" -> "value11", "key12" -> 123L, "key13" -> JsNull),
        "tata" -> "key2",
        "key3" -> true,
        "key4" -> Json.arr("value41", 345.6, "test", Json.obj("key411" -> Json.obj("key4111" -> 987.654)))
      )

      d11.updatePathNode( (path, node) => JsPathExtension.hasKey(path) match {
        case Some(key) => KeyNode("tata", JsString(key))
        case None      => node
      }).root.value must beEqualTo(jsAfter4)

    }

    "insertLeft" in {
      val arr = Json.arr("toto", 123L, false)
      val zipper1 = JsZipper(arr)
      zipper1.insertValueLeft(JsNumber(123.45)) must throwA[RuntimeException]

      zipper1.down.insertValueLeft(JsNumber(123.45)).up.focus must beEqualTo(Node(Json.arr(123.45, "toto", 123L, false)))
      zipper1.down.right.insertValueLeft(JsNumber(123.45)).up.focus must beEqualTo(Node(Json.arr("toto", 123.45, 123L, false)))
      zipper1.down.right.insertValueLeft(JsNumber(123.45)).up.focus must beEqualTo(Node(Json.arr("toto", 123.45, 123L, false)))

      val obj = Json.obj("key1" -> Json.obj("key11" -> "value11", "key12" -> 123L, "key13" -> JsNull))
      val zipper2 = JsZipper(obj)
      zipper2.insertValueLeft(JsString("toto")) must throwA[RuntimeException] 
      zipper2.down.down.insertValueLeft(JsString("toto")) must beEqualTo(JsZipper.Error( __ \ "key1" \ "key11" -> "can't add a value to JsObject, expects KeyNode(String, JsValue)"))
      zipper2.down.down.insertKeyValue("key10" -> JsString("toto")).up.focus must beEqualTo(KeyNode("key1", Json.obj("key10" -> "toto", "key11" -> "value11", "key12" -> 123L, "key13" -> JsNull)))

      zipper1.down.right.insertLeftPathNode{ (path, node) => 
        JsPathExtension.hasIdx(path) match {
          case Some(idx) => Node(JsString(node.value.toString + idx))
          case None      => Node.Empty
        }
      }.root.value must beEqualTo(Json.arr("toto", "1231", 123L, false))
    }

    "insertRight" in {
      val arr = Json.arr("toto", 123L, false)
      val zipper1 = JsZipper(arr)
      zipper1.insertValueRight(JsNumber(123.45)) must throwA[RuntimeException]

      zipper1.down.insertValueRight(Json.obj("toto" -> "tata")).root.value must beEqualTo(Json.arr("toto", Json.obj("toto" -> "tata"), 123L, false))
      zipper1.down.right.insertValueRight(Json.arr(1, 2, 3)).root.value must beEqualTo(Json.arr("toto", 123L, Json.arr(1, 2 ,3), false))
      zipper1.down.right.right.insertValueRight(JsNumber(123.45)).root.value must beEqualTo(Json.arr("toto", 123L, false, 123.45))

      val obj = Json.obj("key1" -> Json.obj("key11" -> "value11", "key12" -> 123L, "key13" -> JsNull))
      val zipper2 = JsZipper(obj)
      zipper2.insertValueRight(JsString("toto")) must throwA[RuntimeException] 
      zipper2.down.down.insertValueRight(JsString("toto")) must beEqualTo(JsZipper.Error( __ \ "key1" \ "key11" -> "can't add a value to JsObject, expects KeyNode(String, JsValue)"))
      zipper2.down.down.insertKeyValue("key10" -> JsString("toto")).up.focus must beEqualTo(KeyNode("key1", Json.obj("key11" -> "value11", "key10" -> "toto", "key12" -> 123L, "key13" -> JsNull)))

      zipper1.down.right.insertRightPathNode{ (path, node) => 
        JsPathExtension.hasIdx(path) match {
          case Some(idx) => Node(JsString(node.value.toString + idx))
          case None      => Node.Empty
        }
      }.root.value must beEqualTo(Json.arr("toto", 123L, "1231", false))
    }

    "delete" in {
      val arr = Json.arr("toto", 123L, false)
      val zipper = JsZipper(arr)
      zipper.delete must beEqualTo(JsZipper.Empty)
      zipper.down.delete.root.value must beEqualTo(Json.arr(123L, false))
      zipper.down.delete.delete.root.value must beEqualTo(Json.arr(false))

      val arr1 = Json.arr("single")
      val arr3 = Json.arr("item1", "item2", "item3")
      JsZipper(arr1).down.delete.root.value must beEqualTo(Json.arr())
      JsZipper(arr3).down.right.delete.root.value must beEqualTo(Json.arr("item1", "item3"))
      JsZipper(arr3).down.right.right.delete.root.value must beEqualTo(Json.arr("item1", "item2"))

      val obj1 = Json.obj("single" -> "value")
      val obj3 = Json.obj("key1" -> "value1", "key2" -> "value2", "key3" -> "value3")
      JsZipper(obj1).down.delete.root.value must beEqualTo(Json.obj())
      JsZipper(obj3).down.delete.root.value must beEqualTo(Json.obj("key2" -> "value2", "key3" -> "value3"))
      JsZipper(obj3).down.right.delete.root.value must beEqualTo(Json.obj("key1" -> "value1", "key3" -> "value3"))
      JsZipper(obj3).down.right.right.delete.root.value must beEqualTo(Json.obj("key1" -> "value1", "key2" -> "value2"))

      val arr1InObj = Json.obj("key1" -> arr1)
      val arr1InArr = Json.arr(arr1)
      JsZipper(arr1InObj).down.down.delete.root.value must beEqualTo(Json.obj("key1" -> Json.arr()))
      JsZipper(arr1InArr).down.down.delete.root.value must beEqualTo(Json.arr(Json.arr()))

      val obj1InObj = Json.obj("key1" -> obj1)
      val obj1InArr = Json.arr(obj1)
      JsZipper(obj1InObj).down.down.delete.root.value must beEqualTo(Json.obj("key1" -> Json.obj()))
      JsZipper(obj1InArr).down.down.delete.root.value must beEqualTo(Json.arr(Json.obj()))
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

      val zipper = JsZipper(js)
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

    "find by path" in {
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
  
      val zipper = JsZipper(js)

      val a = zipper.findPath( __ \ 'key1 \ 'key11 )
      val b = zipper.bottomLeft

      a.focus must beEqualTo(b.focus)
      a.path must beEqualTo(b.path)
      //println(zipper.streamWideFRD.toList.mkString("\n\n"))
    }

    "find by path/node" in {
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
  
      val zipper = JsZipper(js)

      val res = zipper.findByPathNode( (path, node) => path == (__ \ 'key4)(0) && node.value == JsString("TO_FIND") ) 
      val witness = zipper.findPath( (__ \ 'key4)(0) )
      res.focus must beEqualTo(witness.focus)
      res.path must beEqualTo(witness.path)
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

      JsZipper(js).findAllByValue( _ == JsString("TO_FIND") ).map(_.pathValue).toList must beEqualTo(
        List(
          __ \ 'key1 \ 'key11 -> JsString("TO_FIND"),
          (__ \ 'key4)(0) -> JsString("TO_FIND"),
          (__ \ 'key4)(3) \ 'key411 \ 'key4111 -> JsString("TO_FIND")
        )
      )
      JsZipper(js).findAllByPathValue{ (path, value) => 
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

      val zipper = JsZipper(js)

      val res = zipper.findByValue( _ == JsString("TO_FIND") ).update{ js => val JsString(str) = js; JsString(str + "2") }.root
      res.focus.value must beEqualTo(Json.obj(
        "key1" -> Json.obj(
          "key11" -> "TO_FIND2", 
          "key12" -> 123L, 
          "key13" -> JsNull
        ),
        "key2" -> 123,
        "key3" -> true,
        "key4" -> Json.arr("TO_FIND", 345.6, "test", Json.obj("key411" -> Json.obj("key4111" -> "TO_FIND")))
      ))
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

      var zipper = JsZipper(js).findByValue( _ == JsString("TO_FIND") )
      var res = zipper
      while(!zipper.isEmpty) {
        res = zipper.update{ js => val JsString(str) = js; JsString(str + "2") }
        zipper = res.findNextByValue( _ == JsString("TO_FIND") )
      }

      println(res.root)
    }

    "foreach" in {
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

      JsZipper(js).foreach( x => println("foreach:"+x) )
    }

    "withFilter/mapThrough" in {
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

      JsZipper(js)
          .withFilter( zipper => zipper.value == JsString("TO_FIND") )
          .mapThrough( zipper => zipper.update{ js => val JsString(str) = js; JsString(str + "2") })
          .last
          .root
          .value must beEqualTo(
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

    "update by path" in {
      JsZipper(js).createOrUpdate(
        __ \ "key1" \ "key11" -> JsString("toto"),
        __ \ "key1" \ "key12" -> JsNumber(234),
        (__ \ "key4")(0)      -> JsBoolean(true),
        (__ \ "key4")(2)      -> Json.arr(1, 2, 3)
      ).value must beEqualTo(
        Json.obj(
          "key1" -> Json.obj("key11" -> "toto", "key12" -> 234, "key13" -> JsNull),
          "key2" -> 123,
          "key3" -> true,
          "key4" -> Json.arr(true, 345.6, Json.arr(1, 2, 3), Json.obj("key411" -> Json.obj("key4111" -> 987.654)))
        )
      )

    }


    "createOrUpdate path" in {
      val zipper = JsZipper(js)

      zipper.createOrUpdatePath(
        __ \ "key1" \ "key11",
        { js => 
          val JsString(str) = js
          JsString(str+ "_toto")
        }
      ).value must beEqualTo(Json.obj(
        "key1" -> Json.obj("key11" -> "value11_toto", "key12" -> 123L, "key13" -> JsNull),
        "key2" -> 123,
        "key3" -> true,
        "key4" -> Json.arr("value41", 345.6, "test", Json.obj("key411" -> Json.obj("key4111" -> 987.654)))
      ))

      zipper.createOrUpdatePath(
        __ \ "key2",
        { js => 
          val JsNumber(nb) = js
          JsNumber(nb + 5)
        }
      ).value must beEqualTo(Json.obj(
        "key1" -> Json.obj("key11" -> "value11", "key12" -> 123L, "key13" -> JsNull),
        "key2" -> 128,
        "key3" -> true,
        "key4" -> Json.arr("value41", 345.6, "test", Json.obj("key411" -> Json.obj("key4111" -> 987.654)))
      ))
      
      zipper.createOrUpdatePath(
        (__ \ "key4")(1),
        { js => 
          val JsNumber(nb) = js
          JsNumber(nb + 3.0)
        }
      ).value must beEqualTo(Json.obj(
        "key1" -> Json.obj("key11" -> "value11", "key12" -> 123L, "key13" -> JsNull),
        "key2" -> 123,
        "key3" -> true,
        "key4" -> Json.arr("value41", 348.6, "test", Json.obj("key411" -> Json.obj("key4111" -> 987.654)))
      ))

      zipper.createOrUpdatePath(
        (__ \ "key4")(3) \ "key411" \ "key4111",
        { js => 
          val JsNumber(nb) = js
          JsNumber(nb + 3.0)
        }
      ).value must beEqualTo(Json.obj(
        "key1" -> Json.obj("key11" -> "value11", "key12" -> 123L, "key13" -> JsNull),
        "key2" -> 123,
        "key3" -> true,
        "key4" -> Json.arr("value41", 345.6, "test", Json.obj("key411" -> Json.obj("key4111" -> 990.654)))
      ))

      zipper.createOrUpdatePath(
        __,
        { js => 
          JsString("toto")
        }
      ).value must beEqualTo(JsString("toto"))

      zipper.createOrUpdatePath(
        (__ \ "key4")(5),
        { _ => JsBoolean(true) }
      ).value must beEqualTo(Json.obj(
        "key1" -> Json.obj("key11" -> "value11", "key12" -> 123L, "key13" -> JsNull),
        "key2" -> 123,
        "key3" -> true,
        "key4" -> Json.arr("value41", 345.6, "test", Json.obj("key411" -> Json.obj("key4111" -> 987.654)), true)
      ))
    }

    "create from scratch" in {
      JsZipper(Json.obj())
        .createOrUpdatePath( __ \ "key1" \ "key11", _ => JsString("toto") ).value must beEqualTo(
          Json.obj("key1" -> Json.obj("key11" -> "toto"))
        )

      JsZipper(Json.obj())
        .createOrUpdatePath( (__ \ "key1")(0) \ "key11", _ => JsString("toto") ).value must beEqualTo(
          Json.obj("key1" -> Json.arr(Json.obj("key11" -> "toto")))
        )

      JsZipper(Json.obj())
        .createOrUpdatePath( (__ \ "key1")(1) \ "key11", _ => JsNumber(123L) ).value must beEqualTo(
          Json.obj("key1" -> Json.arr(Json.obj("key11" -> 123L)))
        )

      JsZipper(Json.arr())
        .createOrUpdatePath( __(0) \ "key1" \ "key11", _ => JsNumber(123L) ).value must beEqualTo(
          Json.arr(Json.obj("key1" -> Json.obj("key11" -> 123L)))
        )
    }

    "build" in {
      JsZipper.buildJsObject( 
        __ \ "key1" \ "key11" -> JsString("toto"),
        __ \ "key1" \ "key12" -> JsNumber(123L),
        (__ \ "key2")(0)      -> JsBoolean(true),
        __ \ "key3"           -> Json.arr(1, 2, 3)
      ).value must beEqualTo(
        Json.obj(
          "key1" -> Json.obj(
            "key11" -> JsString("toto"),
            "key12" -> JsNumber(123L)
          ),
          "key2" -> Json.arr(true),
          "key3" -> Json.arr(1, 2, 3)
        ))

      JsZipper.buildJsArray(
         __(0) -> JsNumber(123.45),
         __(1) -> JsString("toto")
      ).value must beEqualTo(
        Json.arr(123.45, "toto")
      )
    }
    
  }
}

