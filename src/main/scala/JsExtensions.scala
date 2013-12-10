/**
  * Copyright 2013 Pascal Voitot (@mandubian)
  *
  * Licensed under the Apache License, Version 2.0 (the "License");
  * you may not use this file except in compliance with the License.
  * You may obtain a copy of the License at
  *
  *     http://www.apache.org/licenses/LICENSE-2.0
  *
  * Unless required by applicable law or agreed to in writing, software
  * distributed under the License is distributed on an "AS IS" BASIS,
  * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
  * See the License for the specific language governing permissions and
  * limitations under the License.
  */
package play.api.libs.json

package object extensions {

implicit class JsExtensions(val js: JsValue) extends AnyVal {

  def get(path: JsPath): JsValue = {
    JsZipper(js).findPath(path).value
  }

  def getOpt(path: JsPath): Option[JsValue] = {
    JsZipper(js).findPath(path) match {
      case JsZipper.Empty => None
      case zipper => Some(zipper.value)
    }
  }

  def set(pathValues: (JsPath, JsValue)*): JsValue = {
    JsZipper(js).createOrUpdate(pathValues).root.value
  }

  def delete(path: JsPath): JsValue = {
    JsZipper(js).findPath(path).delete.root.value
  }

  def delete(path1: JsPath, path2: JsPath, others: JsPath*): JsValue = {
    val paths = path1 +: path2 +: others.toList
    JsZipper(js)
      .deletePaths(paths).value
  }

  def update(path: JsPath, f: JsValue => JsValue): JsValue = {
    JsZipper(js).findPath(path) match {
      case JsZipper.Empty => js
      case zip => zip.update(f).root.value
    }
  }

  def findAllByValue(f: JsValue => Boolean): Stream[(JsPath, JsValue)] = {
    JsZipper(js).findAllByValue(f) map { zipper => zipper.pathValue }
  }

  def findAll(f: (JsPath, JsValue) => Boolean): Stream[(JsPath, JsValue)] = {
    JsZipper(js).findAllByPathValue(f) map { zipper => zipper.pathValue }
  }

  def updateAllByValue(mapF: PartialFunction[JsValue, JsValue]): JsValue = {
    JsZipper(js).filterMapThroughByValue{ mapF.isDefinedAt(_) }(mapF).root.value
  }

  def updateAll(mapF: PartialFunction[(JsPath, JsValue), JsValue]): JsValue = {
    JsZipper(js).filterMapThrough{ zipper => mapF.isDefinedAt(zipper.path, zipper.value) }{ zipper =>
      zipper.updatePathNode( (path, node) => Node.copy(node, mapF(path, node.value)) )
    }.root.value
  }

  def updateAllKeyNodes(mapF: PartialFunction[(JsPath, JsValue), (String, JsValue)]): JsValue = {
    JsZipper(js).filterMapThrough{ zipper => mapF.isDefinedAt(zipper.path, zipper.value) }{ zipper =>
      zipper.updatePathNode{ (path, node) =>
        Node.copyKeyNode(node, mapF(path, node.value))
      }
    }.root.value
  }

  def filterUpdateAllByValue(filterF: JsValue => Boolean)(mapF: JsValue => JsValue): JsValue = {
    JsZipper(js).filterMapThroughByValue(filterF)(mapF).root.value
  }

  def filterUpdateAll(filterF: (JsPath, JsValue) => Boolean)(mapF: (JsPath, JsValue) => JsValue): JsValue = {
    JsZipper(js).filterMapThrough{ zipper =>
      filterF(zipper.path, zipper.value)
    }{ zipper =>
      zipper.updatePathNode( (path, node) => Node.copy(node, mapF(path, node.value)) )
    }.root.value
  }

  /** Monadic features */
  import monad._, syntax._

  def setM[M[_]: Monad](pathValues: (JsPath, M[JsValue])*): M[JsValue] = {
    JsZipperM[M](js).createOrUpdate(pathValues).map(_.root.value)
  }

  def findAllByValueM[M[_]: Monad](f: JsValue => Boolean): Stream[(JsPath, JsValue)] = {
    JsZipperM[M](js).findAllByValue(f) map { zipper => zipper.pathValue }
  }

  def findAllM[M[_]: Monad](f: (JsPath, JsValue) => Boolean): Stream[(JsPath, JsValue)] = {
    JsZipperM[M](js).findAllByPathValue(f) map { zipper => zipper.pathValue }
  }

  def filterUpdateAllByValueM[M[_]: Monad](filterF: JsValue => Boolean)(mapF: JsValue => M[JsValue]): M[JsValue] = {
    JsZipperM[M](js).filterMapThrough{ (zipper:JsZipperM[M]) =>
      filterF(zipper.value)
    }{ (zipper:JsZipperM[M]) =>
      zipper.update(mapF)
    }.map(_.root.value)
  }

  def filterUpdateAllM[M[_]: Monad](filterF: (JsPath, JsValue) => Boolean)(mapF: (JsPath, JsValue) => M[JsValue]): M[JsValue] = {
    JsZipperM[M](js).filterMapThrough{ (zipper:JsZipperM[M]) =>
      filterF(zipper.path, zipper.value)
    }{ zipper =>
      zipper.updatePathNode( (path, node) => mapF(path, node.value).map{ value => Node.copy(node, value) } )
    }.map(_.root.value)
  }

  def updateAllByValueM[M[_]: Monad](mapF: JsValue => M[JsValue]): M[JsValue] = {
    JsZipperM[M](js).filterMapThrough{ _:JsZipperM[M] => true }{ (zipper:JsZipperM[M]) =>
      zipper.update(mapF)
    }.map(_.root.value)
  }

  def updateAllM[M[_]: Monad](mapF: (JsPath, JsValue) => M[JsValue]): M[JsValue] = {
    JsZipperM[M](js).filterMapThrough{ _:JsZipperM[M] => true }{ zipper =>
      zipper.updatePathNode( (path, node) => mapF(path, node.value).map{ value => Node.copy(node, value) } )
    }.map(_.root.value)
  }
}

object JsExtensions {
  import monad._, syntax._

  def buildJsObject(pathValues: (JsPath, JsValue)*): JsValue =
    JsZipper.buildJsObject(pathValues).root.value

  def buildJsArray(values: JsValue*): JsValue =
    JsZipper.buildJsArray(
      values.zipWithIndex.map{ case (v, idx) => __(idx) -> v }
    ).root.value

  def buildJsObjectM[M[_]:Monad](pathValues: (JsPath, M[JsValue])*): M[JsValue] =
    JsZipperM.buildJsObject(pathValues).map(_.root.value)

  def buildJsArrayM[M[_]:Monad](values: M[JsValue]*): M[JsValue] =
    JsZipperM.buildJsArray(
      values.zipWithIndex.map{ case (v, idx) => __(idx) -> v }
    ).map(_.root.value)

}

object \ {
  def unapply(p: JsPath): Option[(JsPath, String)] = p.path.reverse match {
    case KeyPathNode(key) :: tail => Some(JsPath(tail.reverse) -> key)
    case _                        => None
  }
}

object \?\ {
  def unapply(p: JsPath): Option[(JsPath, String)] = p.path match {
    case (h@KeyPathNode(head)) :: (i@IdxPathNode(idx)) :: tail => tail.reverse match {
      case (t @ KeyPathNode(last)) :: middle => Some((JsPath(List(h, i)), last))
      case _                                 => None
    }
    case (h@KeyPathNode(head)) :: tail => tail.reverse match {
      case (t @ KeyPathNode(last)) :: middle => Some((JsPath(List(h)), last))
      case _                                 => None
    }
    case _ => None
  }
}

object @@ {
  def unapply(p: JsPath): Option[(JsPath, Int)] = p.path.reverse match {
    case IdxPathNode(idx) :: tail =>
      Some( (JsPath(tail.reverse) -> idx ) )
    case _                        => None
  }
}

object ReadsMatch {
  def unapply[T:Reads](js: JsValue): Option[T] = implicitly[Reads[T]].reads(js).asOpt
}

/** Stolen from Rapture.io because until nested object are allowed in subsequent scala releases,
  * it's impossible to nest an object in a value class
  */
@inline implicit class JsonHelper(val sc: StringContext) extends {
  object json{
    def apply(args: Any*): JsValue = {
      val strings = sc.parts.iterator
      val expressions = args.iterator
      var buf = new StringBuffer(strings.next)
      while(strings.hasNext) {
        buf append expressions.next
        buf append strings.next
      }
      Json.parse(buf.toString)
    }

    def compare(matcher: JsZipper, zipper: JsZipper): Option[Seq[JsValue]] = {
      def step(acc: Seq[Option[JsValue]], curMatcher: JsZipper, curZipper: JsZipper): Seq[Option[JsValue]] = {
        (curMatcher.value, curZipper.value) match {
          case (JsString("_?_"), a) => curZipper.right match {
            case JsZipper.Empty => acc :+ Some(a)
            case right => step(acc :+ Some(a), curMatcher.right, right)
          }

          case (a, b) if a == b => curZipper.right match {
            case JsZipper.Empty => acc
            case right => step(acc, curMatcher.right, right)
          }

          case (a, b) => curZipper.right match {
            case JsZipper.Empty => if(curZipper.isLeaf) acc :+ None
                                   else step(acc, curMatcher.down.first, curZipper.down.first)
            case right => if(curZipper.isLeaf) step(acc :+ None, curMatcher.right, right)
                          else step(acc, curMatcher.down.first, curZipper.down.first)
          }
        }
      }

      val matching = step(Seq(), matcher, zipper)

      if(matching.exists( _.isEmpty )) None
      else Some(matching.map(_.get))
    }

    val VAR_REPLACE = "\"_?_\""

    def unapplySeq(js: JsValue): Option[Seq[JsValue]] = {
      val jsBase = Json.parse(sc.parts.mkString(VAR_REPLACE))

      compare(JsZipper(jsBase), JsZipper(js))
      /*val witness = JsZipper(jsBase).streamDeepLeaves
      val sample = JsZipper(js).streamDeepLeaves

      val zipped = sample.zip(witness)
      val matching = zipped.foldLeft(Seq[Option[JsValue]]()){ case( all, (zipsample, zipwitness)) =>
        (zipsample.value, zipwitness.value) match {
          case (a, JsNull)      => all :+ Some(a)
          case (a, b) if a == b => all
          case _                => all :+ None
        }
      }

      println("matching:"+matching)

      if(matching.exists( _.isEmpty )) None
      else Some(matching.map(_.get))*/

    }
  }

}

}