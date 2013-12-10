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

import scala.util.{Try, Success, Failure}
import scala.annotation.tailrec

trait JsZipper {
  import JsZipper._

  def focus:   Node
  def lefts:   Siblings
  def rights:  Siblings
  def parents: Parents

  def value = focus.value

  def isPlain = this match {
    case _:JsZipperEmpty => false
    case _:JsZipperError => false
    case _ => true
  }

  def isEmpty = this match {
    case _:JsZipperEmpty => true
    case _ => false
  }

  def isError = this match {
    case _:JsZipperError => true
    case _ => false
  }

  def isObject = this.isPlain && (focus.value match {
    case _: JsObject => true
    case _           => false
  })

  def isArray = this.isPlain && (focus.value match {
    case _: JsArray  => true
    case _           => false
  })

  def isLeaf = this.isPlain && (focus.value match {
    case _: JsObject => false
    case _: JsArray  => false
    case _           => true
  })

  def down: JsZipper = focus.value match {
    case obj: JsObject => obj.fields.toList match {
      case Nil => JsZipper.Empty
      case (key, value) :: tail =>
        JsZipper(
          Node(key, value),
          Stream.empty,
          objTailToStream(tail),
          (lefts, focus, rights) #:: parents
        )
    }

    case arr: JsArray => arr.value.toList match {
      case Nil => JsZipper.Empty
      case head :: tail =>
        JsZipper(
          Node(head),
          Stream.empty,
          arrayTailToStream(tail),
          (lefts, focus, rights) #:: parents)
    }

    case v => JsZipper.Empty
  }

  def left: JsZipper = lefts match {
    case head #:: tail   => JsZipper(head, tail, focus #:: rights, parents)
    case Stream.Empty    => JsZipper.Empty
  }

  def right: JsZipper = rights match {
    case head #:: tail   => JsZipper(head, focus #:: lefts, tail, parents)
    case Stream.Empty    => JsZipper.Empty
  }

  def up: JsZipper = parents match {
    case (plefts, parent, prights) #:: ancestors =>
      val js = reify(focus, lefts, rights, parent)
      JsZipper (
        Node.copy(parent, js),
        plefts, prights, ancestors
      )
    case Stream.Empty => JsZipper.Empty
  }

  def first: JsZipper = {
    def loop(zip: JsZipper): JsZipper = zip.left match {
      case _:JsZipperEmpty => zip
      case lft => loop(zip.left)
    }
    loop(this)
  }

  def last: JsZipper = {
    def loop(zip: JsZipper): JsZipper = zip.right match {
      case _:JsZipperEmpty => zip
      case lft => loop(zip.right)
    }
    loop(this)
  }

  def root: JsZipper = up match {
    case _:JsZipperEmpty   => this
    case e:JsZipper.Error  => e
    case parent            => parent.root
  }

  def bottomLeft: JsZipper = {
    def loop(zip: JsZipper): JsZipper = zip.down match {
      case _:JsZipperEmpty => zip
      case dwn => loop(dwn)
    }
    loop(this)
  }

  def bottomRight: JsZipper = {
    def loop(zip: JsZipper): JsZipper = zip.last.down match {
      case _:JsZipperEmpty => zip
      case dwn => loop(dwn)
    }
    loop(this)
  }

  def parent: Node = parents match {
    case (_, father, _) #:: _ => father
    case Stream.Empty         => Node.Empty
  }

  def path: JsPath = pathSafe.get

  def pathSafe: Option[JsPath] = {
    def loop(node: Node, prts: Parents, lfts: Siblings, path: JsPath): Option[JsPath] = {
      prts match {
        case (plefts, prt, _) #:: ancestors =>
          localPath(JsPath(), node, prt, lfts).flatMap( p => loop(prt, ancestors, plefts, p ++ path) )
        case Stream.Empty =>
          localPath(JsPath(), node, Node.Empty, lefts).map( _ ++ path )
      }
    }

    loop(focus, parents, lefts, JsPath())
  }

  def orphanize: JsZipper = JsZipper(focus, lefts, rights, Stream.Empty)

  def update(js: JsValue): JsZipper = updateNode( (_:Node) => Node.copy(focus, js) )
  def update(fn: JsValue => JsValue): JsZipper = updateNode( (node:Node) => Node.copy(focus, fn(focus.value)) )
  def updatePathNode(fn: (JsPath, Node) => Node): JsZipper = updateNode( (n:Node) => fn(path, n) )
  def updateNode(fn: Node => Node): JsZipper = this match {
    case JsZipper.Empty => JsZipper.Empty
    case _ => (focus, fn(focus)) match {
      case (KeyNode(_, _), KeyNode(k2, v)) =>
        JsZipper(KeyNode(k2, v), lefts, rights, parents)
      case (PlainNode(_), PlainNode(v)) =>
        JsZipper(PlainNode(v), lefts, rights, parents)
      case (_, Node.Empty) => this // unchanged
      case _ => JsZipper.Error(path -> "Can't update node (key+value or value) to another type of node")
    }
  }

  def insertValueLeft(js: JsValue): JsZipper = insertLeftNode( (_: Node) => Node(js) )
  def insertValueLeft(fn: JsValue => JsValue): JsZipper = insertLeftNode( (_: Node) => Node(fn(focus.value)) )

  def insertLeftPathNode(fn: (JsPath, Node) => Node): JsZipper = insertLeftNode( (n:Node) => fn(path, n) )

  def insertLeftNode(fn: Node => Node): JsZipper = parent match {
    case Node(parent) => parent match {
      case obj: JsObject =>
        fn(focus) match {
          case KeyNode(key, value) =>
            JsZipper(
              focus,
              Node(key, value) #:: lefts,
              rights,
              parents
            )
          case Node.Empty => this // unchanged
          case _ => JsZipper.Error(path -> "can't add a value to JsObject, expects KeyNode(String, JsValue)")
        }

      case arr: JsArray  =>
        fn(focus) match {
          case PlainNode(v) =>
            JsZipper(
              focus,
              Node(v) #:: lefts,
              rights,
              parents
            )
          case Node.Empty => this // unchanged
          case _ => JsZipper.Error(path -> "can't add a (key, value) to JsArray, expects PlainNode(JsValue)")
        }

      case _ => sys.error("Can't have multiple JsValues on root")
    }

    case Node.Empty => JsZipper.Error(path -> "Can't have multiple JsValues on root")
  }

  def insertValueRight(js: JsValue): JsZipper = insertRightNode( (_: Node) => Node(js) )
  def insertValueRight(fn: JsValue => JsValue): JsZipper = insertRightNode( (_: Node) => Node(fn(focus.value)) )

  // By default inserting a key/value to JsObject is done on the right
  def insertKeyValue(kv: (String, JsValue)): JsZipper = insertRightNode( (_: Node) => Node(kv._1, kv._2) )
  def insertKeyValue(fn: (String, JsValue) => (String, JsValue)): JsZipper =
    insertRightNode{ (n: Node) =>
      n match {
        case KeyNode(key, value) => val (k, v) = fn(key, value)
                                    Node(k, v)

        case _                   => Node.Empty
      }
  }

  def insertRightPathNode(fn: (JsPath, Node) => Node): JsZipper = insertRightNode( (n:Node) => fn(path, n) )

  def insertRightNode(fn: Node => Node): JsZipper = parent match {
    case Node(parent) => parent match {
      case obj: JsObject =>
        fn(focus) match {
          case KeyNode(key, value) =>
            JsZipper(
              focus,
              lefts,
              Node(key, value) #:: rights,
              parents
            )
          case Node.Empty => this // unchanged
          case _ => JsZipper.Error(path -> "can't add a value to JsObject, expects KeyNode(String, JsValue)")
        }

      case arr: JsArray  =>
        fn(focus) match {
          case PlainNode(value) =>
            JsZipper(
              focus,
              lefts,
              Node(value) #:: rights,
              parents
            )
          case Node.Empty => this // unchanged
          case _ => JsZipper.Error(path -> "can't add a (key, value) to JsArray, expects PlainNode(JsValue)")
        }

      case _ => sys.error("Can't have multiple JsValues on root")
    }

    case Node.Empty => JsZipper.Error(path -> "Can't have multiple JsValues on root")
  }

  def insertDown(node: Node): JsZipper = value match {
    case obj: JsObject =>
      if(obj.fields.isEmpty) node match {
        case KeyNode(key, value) =>
          JsZipper(
            node,
            Stream.Empty,
            Stream.Empty,
            (lefts, focus, rights) #:: parents
          )
        case _ => sys.error("Can't insert/down value in JsObject without key")
      }
      else down.last.insertRightNode(_ => node)

    case arr: JsArray =>
      if(arr.value.isEmpty) node match {
        case PlainNode(value) =>
          JsZipper(
            node,
            Stream.Empty,
            Stream.Empty,
            (lefts, focus, rights) #:: parents
          )
        case _ => sys.error("Can't insert/down key/value in JsArray")
      }
      else down.last.insertRightNode(_ => node)

    case _ => sys.error("Can't insert/down in value")
  }

  def delete: JsZipper = rights match {
    case r #:: righters => parent match {
      case Node(parent) => parent match {
        case arr: JsArray =>
          JsZipper(
            r,
            lefts,
            righters,
            parents
          )
        case _ =>
          JsZipper(r, lefts, righters, parents)
      }

      case Node.Empty => sys.error("Can't have multiple JsValues on root")
    }

    case Stream.Empty  => lefts match {
      case l #:: lefters => JsZipper(l, lefters, Stream.empty, parents)
      case Stream.Empty  => parents match {
        case (plefts, parent, prights) #:: ancestors =>
          val emptyValue = parent match {
            case Node(pnode) => pnode match {
              case JsArray(_)  => Json.arr()
              case JsObject(_) => Json.obj()
              case _           => sys.error("A simple value can't have children")
            }
          }
          val pnode = parent match {
            case KeyNode(key, _) => Node(key, emptyValue)
            case PlainNode(_)    => Node(emptyValue)
            case _               => sys.error("A simple value can't have children")
          }

          JsZipper(pnode, plefts, prights, ancestors)

        case Stream.Empty => JsZipper.Empty
      }
    }
  }

  def orElse(other: JsZipper) = this match {
    case JsZipper.Empty => other
    case t              => t
  }

  /* Horizontal streams */
  def streamLeft: Stream[JsZipper]  =
    this.left match {
      case JsZipper.Empty => Stream.Empty
      case zip => zip #:: zip.streamLeft
    }

  def streamRight: Stream[JsZipper] =
    this.right match {
      case JsZipper.Empty => Stream.Empty
      case zip => zip #:: zip.streamRight
    }

  /* Vertical streams */
  def streamDown: Stream[JsZipper]  =
    this.down match {
      case JsZipper.Empty => Stream.Empty
      case zip => zip #:: zip.streamDown
    }

  def streamUp: Stream[JsZipper]  =
    this.up match {
      case JsZipper.Empty => Stream.Empty
      case zip => zip #:: zip.streamUp
    }

  /* In depth streams */
  def streamDeepRightFocusUp: Stream[JsZipper] = {
    this.right match {
      case JsZipper.Empty =>
        this.up match {
          case JsZipper.Empty => Stream(this)
          case lup            => this #:: lup.streamDeepRightFocusUp
        }
      case rgt => this #:: rgt.streamDeepLeftFocusRightUp
    }
  }
  def streamDeepRFU: Stream[JsZipper] = streamDeepRightFocusUp

  def streamDeepLeftFocusRightUp: Stream[JsZipper] = {
    if(isLeaf) streamDeepRightFocusUp
    else bottomLeft.streamDeepRightFocusUp
  }
  def streamDeepLFRU: Stream[JsZipper] = streamDeepLeftFocusRightUp

  def streamDeepLeftFocusRightUp(filter: JsZipper => Boolean)(map: JsZipper => JsZipper): Stream[JsZipper] = {
    if(isLeaf) streamDeepRightFocusUp(filter)(map)
    else bottomLeft.streamDeepRightFocusUp(filter)(map)
  }

  def streamDeepRightFocusUp(filter: JsZipper => Boolean)(map: JsZipper => JsZipper): Stream[JsZipper] = {
    if(filter(this)) {
      val z = map(this)
      z.right match {
        case JsZipper.Empty =>
          z.up match {
            case JsZipper.Empty => Stream(z)
            case lup            => z #:: lup.streamDeepRightFocusUp(filter)(map)
          }
        case rgt => z #:: rgt.streamDeepLeftFocusRightUp(filter)(map)
      }
    }
    else {
      this.right match {
        case JsZipper.Empty =>
          this.up match {
            case JsZipper.Empty => Stream.Empty
            case lup            => lup.streamDeepRightFocusUp(filter)(map)
          }
        case rgt => rgt.streamDeepLeftFocusRightUp(filter)(map)
      }
    }
  }

  def streamDeepLeaves = streamDeepLeftFocusRightUp.filter(_.isLeaf)

  /* In width streams */
  def streamWideFocusRightDown: Stream[JsZipper] = {
    this match {
      case JsZipper.Empty => Stream.Empty
      case zip            =>
        val str = zip #:: zip.streamRight
        // perf ++ ???
        str ++ str.flatMap( _.down.streamWideFocusRightDown )
    }
  }
  def streamWideFRD: Stream[JsZipper] = streamWideFocusRightDown

  def find(fn: JsZipper => Boolean): JsZipper =
    streamDeepLFRU.collectFirst{
      case zipper if zipper.isPlain && fn(zipper) => zipper
    } getOrElse JsZipper.Empty

  def findNext(fn: JsZipper => Boolean): JsZipper =
    // skips this in the stream
    streamDeepRFU.tail.collectFirst{
      case zipper if zipper.isPlain && fn(zipper) => zipper
    } getOrElse JsZipper.Empty

  def findByValue(fn: JsValue => Boolean): JsZipper = findByNode( node => fn(node.value) )
  def findNextByValue(fn: JsValue => Boolean): JsZipper = findNextByNode( node => fn(node.value) )

  def findByNode(fn: Node => Boolean): JsZipper =
    streamDeepLFRU.collectFirst{
      case zipper if zipper.isPlain && fn(zipper.focus) => zipper
    } getOrElse JsZipper.Empty

  def findNextByNode(fn: Node => Boolean): JsZipper =
    // skips this in the stream
    streamDeepRFU.tail.collectFirst{
      case zipper if zipper.isPlain && fn(zipper.focus) => zipper
    } getOrElse JsZipper.Empty

  def findByPathNode(fn: (JsPath, Node) => Boolean): JsZipper =
    streamDeepLFRU.collectFirst{
      case zipper if zipper.isPlain && fn(zipper.path, zipper.focus) => zipper
    } getOrElse JsZipper.Empty

  def findNextByPathNode(fn: (JsPath, Node) => Boolean): JsZipper =
    // skips this in the stream
    streamDeepRFU.tail.collectFirst{
      case zipper if zipper.isPlain && fn(zipper.path, zipper.focus) => zipper
    } getOrElse JsZipper.Empty

  def findAll(fn: JsZipper => Boolean): Stream[JsZipper] =
    streamDeepLFRU.collect{
      case zipper if zipper.isPlain && fn(zipper) => zipper
    }

  def findAllByValue(fn: JsValue => Boolean): Stream[JsZipper] =
    streamDeepLFRU.collect{
      case zipper if zipper.isPlain && fn(zipper.focus.value) => zipper
    }

  def findAllByPathValue(fn: (JsPath, JsValue) => Boolean): Stream[JsZipper] =
    streamDeepLFRU.collect{
      case zipper if zipper.isPlain && fn(zipper.path, zipper.focus.value) => zipper
    }

  def findPath(path: JsPath): JsZipper = {
    @tailrec
    def step(currentPath: List[PathNode], currentZipper: JsZipper): JsZipper = currentPath match {
      case Nil       => currentZipper.up
      case List(p)   => p match {
        case KeyPathNode(pkey) =>
          (currentZipper #:: currentZipper.streamRight).collectFirst{ zipper => zipper.focus match {
            case KeyNode(key, _) if(key == pkey) => zipper
          } } match {
            case None         => JsZipper.Empty // not found
            case Some(zipper) => zipper
          }
        case IdxPathNode(idx) =>
          (currentZipper #:: currentZipper.streamRight).drop(idx) match {
            case Stream.Empty  => JsZipper.Empty // not found
            case head #:: _    => head
          }
        case _ => JsZipper.Empty // TODO (recursive path???)
      }
      case p :: tail =>
        p match {
          case KeyPathNode(pkey) =>
            (currentZipper #:: currentZipper.streamRight).collectFirst{ zipper => zipper.focus match {
              case KeyNode(key, _) if(key == pkey) => zipper
            } } match {
              case None         => JsZipper.Empty // not found
              case Some(zipper) => step(tail, zipper.down)
            }
          case IdxPathNode(idx) =>
            (currentZipper #:: currentZipper.streamRight).drop(idx) match {
              case Stream.Empty  => JsZipper.Empty // not found
              case head #:: _    => step(tail, head.down)
            }
          case _ => JsZipper.Empty // TODO (recursive path???)
        }
    }
    step(path.path, this.down)
    /*streamWideFRD.collectFirst{
      case zipper if zipper.isPlain && zipper.path == path => zipper
    } getOrElse JsZipper.Empty*/
  }

  def createOrUpdatePath(path: JsPath, f: JsValue => JsValue): JsZipper = {
    @tailrec
    def step(currentPath: List[PathNode], currentZipper: JsZipper, parentZipper: JsZipper): JsZipper = currentPath match {
      case Nil       => currentZipper.up.update(f)
      case List(p)   => p match {
        case KeyPathNode(pkey) =>
          currentZipper match {
            case JsZipper.Empty => // inserting in empty object
              parentZipper.insertDown(KeyNode(pkey, f(Json.obj())))
            case _ =>
              (currentZipper #:: currentZipper.streamRight).collectFirst{ zipper => zipper.focus match {
                case KeyNode(key, _) if(key == pkey) => zipper
              } } match {
                case None => // not found so inserting
                  if(currentZipper.parent.isObject) currentZipper.insertKeyValue( pkey -> f(Json.obj()) )
                  else JsZipper.Empty
                case Some(zipper) => zipper.update(f(zipper.value))
              }
          }

        case IdxPathNode(idx) =>
          currentZipper match {
            case JsZipper.Empty => // inserting in empty object
              parentZipper.insertDown(PlainNode(f(Json.arr())))
            case _ =>
              (currentZipper #:: currentZipper.streamRight).drop(idx) match {
                case Stream.Empty | JsZipper.Empty #:: _ => // not found
                  if(currentZipper.parent.isArray) currentZipper.last.insertValueRight(f(Json.arr()))
                  else JsZipper.Empty
                case head #:: _    => head.update(f)
              }
          }
        case _ => JsZipper.Empty // TODO (recursive path???)
      }
      case p :: next :: tail =>
        p match {
          case KeyPathNode(pkey) =>
            (currentZipper #:: currentZipper.streamRight).collectFirst{ zipper => zipper.focus match {
              case KeyNode(key, _) if(key == pkey) => zipper
            } } match {
              case None => // not found
                if(parentZipper.focus.isObject){
                  val n = next match {
                    case _:KeyPathNode => Json.obj()
                    case _:IdxPathNode => Json.arr()
                    case _ => JsNull
                  }
                  if(parentZipper.focus.isEmptyObjArr){
                    val z = parentZipper.insertDown(KeyNode(pkey, n))
                    step(next :: tail, z.down, z)
                  }
                  else {
                    val z = currentZipper.last.insertKeyValue( pkey -> n ).right
                    step(next :: tail, z.down, z)
                  }
                }
                else JsZipper.Empty
              case Some(zipper) => step(next :: tail, zipper.down, zipper)
            }
          case IdxPathNode(idx) =>
            (currentZipper #:: currentZipper.streamRight).drop(idx) match {
              case Stream.Empty | JsZipper.Empty #:: _ => // not found
                if(parentZipper.focus.isArray) {
                  val n = next match {
                    case _:KeyPathNode => Json.obj()
                    case _:IdxPathNode => Json.arr()
                    case _             => JsNull
                  }
                  if(parentZipper.focus.isEmptyObjArr){
                    val z = parentZipper.insertDown(PlainNode(n))
                    step(next :: tail, z.down, z)
                  } else {
                    val z = currentZipper.last.insertValueRight(n).right
                    step(next :: tail, z.down, z)
                  }
                }
                else JsZipper.Empty
              case head #:: _    => step(next :: tail, head.down, head)
            }
          case _ => JsZipper.Empty // TODO (recursive path???)
        }
    }
    step(path.path, this.down, this).root
  }

  def createOrUpdatePath(path: JsPath, js: JsValue): JsZipper = createOrUpdatePath(path, _ => js)

  def createOrUpdate(pathValues: Seq[(JsPath, JsValue)]): JsZipper = {
    @tailrec
    def step(pathValues: List[(JsPath, JsValue)], zipper: JsZipper): JsZipper = {
      pathValues match {
        case Nil          => JsZipper.Empty
        case List(pv)     => zipper.createOrUpdatePath(pv._1, pv._2)
        case head :: tail => step(tail, zipper.createOrUpdatePath(head._1, head._2))
      }
    }

    step(pathValues.toList, this)
  }

  def createOrUpdate(pathValue: (JsPath, JsValue), pathValues: (JsPath, JsValue)*): JsZipper =
    createOrUpdate(Seq(pathValue) ++ pathValues)


  def deletePaths(paths: Seq[JsPath]): JsZipper = {
    @tailrec
    def step(zipper: JsZipper, currPaths: List[JsPath]): JsZipper = {
      currPaths match {
        case Nil           => zipper
        case List(p)       => zipper.findPath(p) match {
          case JsZipper.Empty => zipper.root
          case found          => found.delete.root
        }
        case head :: tail => zipper.findPath(head) match {
          case JsZipper.Empty => zipper.root
          case found          => step(found.delete.root, tail)
        }
      }
    }

    step(this, paths.toList)
  }

  /* mimicing collection but temporary functions */
  def head = this
  def tail = streamDeepLFRU.tail
  def tailDefined = this match {
    case JsZipper.Empty => false
    case _              => true
  }

  def foreach[U](f: JsZipper => U) = streamDeepLFRU.foreach(f)
  //def filter(fn: JsZipper => Boolean): Stream[JsZipper] = findAll(fn)
  def withFilter(fn: JsZipper => Boolean): JsZipperWithFilter = new JsZipperWithFilter(fn)
  def mapThrough(fn: JsZipper => JsZipper): JsZipper = filterMapThrough(_ => true)(fn)
  def mapThroughByValue(mapFn: JsValue => JsValue): JsZipper =
    filterMapThrough( _ => true )( zipper => zipper.update(mapFn) )

  def filterMapThroughByValue(filterFn: JsValue => Boolean)(mapFn: JsValue => JsValue): JsZipper =
    filterMapThrough( zipper => filterFn(zipper.value) )( zipper => zipper.update(mapFn) )

  def filterMapThrough(filterFn: JsZipper => Boolean)(mapFn: JsZipper => JsZipper): JsZipper = {
    @tailrec
    def step(zipper: JsZipper): JsZipper = {
      zipper match {
        case JsZipper.Empty => JsZipper.Empty
        case found          =>
          val updated = mapFn(found)

          updated.findNext(filterFn) match {
            case JsZipper.Empty  => updated
            case found           => step(found)
          }
      }
    }

    find(filterFn) match {
      case JsZipper.Empty => this
      case zipper => step(zipper)
    }
  }

  def filterDeleteThrough(filterFn: JsZipper => Boolean): JsZipper = {
    @tailrec
    def step(zipper: JsZipper): JsZipper = {
      zipper match {
        case JsZipper.Empty => JsZipper.Empty
        case found          =>
          val next = found.delete
          if(filterFn(next)) step(next)
          else next.findNext(filterFn) match {
            case JsZipper.Empty  => next
            case found           => step(found)
          }
      }
    }

    step(find(filterFn))
  }

  final class JsZipperWithFilter(filterFn: JsZipper => Boolean)  {
    def mapThrough(mapFn: JsZipper => JsZipper): JsZipper = JsZipper.this.filterMapThrough(filterFn)(mapFn)
  }

  def pathValue: (JsPath, JsValue) = (path, focus.value)

  override def toString =
    s"""JsZipper(focus=$focus, lefts=$lefts, rights=$rights, parents=$parents)"""
}

trait JsZipperOps {
  def zipStream(js: JsValue): Stream[JsZipper] = JsZipper(js).streamDeepLFRU
}

trait JsZipperEmpty extends JsZipper {
  val focus: Node = Node.Empty
  val lefts: JsZipper.Siblings = Stream.Empty
  val rights: JsZipper.Siblings = Stream.Empty
  val parents: JsZipper.Parents = Stream.Empty

  override def toString =
    s"""JsZipper.Empty(focus=$focus, lefts=$lefts, rights=$rights, parents=$parents)"""
}

trait JsZipperError extends JsZipper {
  val error: (JsPath, String)
  override val focus: Node = Node.Error(error)
  override val lefts: JsZipper.Siblings = Stream.Empty
  override val rights: JsZipper.Siblings = Stream.Empty
  override val parents: JsZipper.Parents = Stream.Empty

  override def toString =
    s"""JsZipper.Error(error: $error, focus=$focus, lefts=$lefts, rights=$rights, parents=$parents)"""
}

object JsZipper extends JsZipperOps {
  type Siblings = Stream[Node]
  type Parent   = (Siblings, Node, Siblings)
  type Parents  = Stream[Parent]

  def apply(js: JsValue) =
    new JsZipper {
      val focus   = Node(js)
      val lefts   = Stream.empty
      val rights  = Stream.empty
      val parents = Stream.empty
    }

  case object Empty extends JsZipperEmpty
  case class Error(override val error: (JsPath, String)) extends JsZipperError

  def apply(theFocus: Node, theLefts: Siblings, theRights: Siblings, theParents: Parents) =
    new JsZipper {
      val focus   = theFocus
      val lefts   = theLefts
      val rights  = theRights
      val parents = theParents
    }

  def unapply(zipper: JsZipper): Option[(Node, Siblings, Siblings, Parents)] =
    Some((zipper.focus, zipper.lefts, zipper.rights, zipper.parents))

  def mergeSiblingsAsObjLeft(siblings: Siblings): JsObject =
    JsObject(siblings.foldLeft(Seq[(String, JsValue)]()){ (seq, n) => n match {
      case KeyNode(key, value) => (key -> value) +: seq
      case _                   => seq
    }})

  def mergeSiblingsAsObj(siblings: Siblings): JsObject =
    JsObject(siblings.reverse.foldLeft(Seq[(String, JsValue)]()){ (seq, n) => n match {
      case KeyNode(key, value) => (key -> value) +: seq
      case _                   => seq
    }})

  def mergeSiblingsAsArrLeft(siblings: Siblings): JsArray =
    siblings.foldLeft(Json.arr()){ (arr, n) => n match {
      case PlainNode(value) => value +: arr
      case _                => arr
    }}

  def mergeSiblingsAsArrRight(siblings: Siblings): JsArray =
    siblings.foldLeft(Json.arr()){ (arr, n) => n match {
      case PlainNode(value) => arr :+ value
      case _                => arr
    }}

  def reify(node: Node, lefts: Siblings, rights: Siblings, parent: Node): JsValue =
    (parent.value, node) match {
      case (JsObject(_), KeyNode(key, value)) =>
        mergeSiblingsAsObjLeft(lefts) +
        (key -> value) ++
        mergeSiblingsAsObj(rights)

      case (JsArray(_), PlainNode(value)) =>
        (mergeSiblingsAsArrLeft(lefts) :+
        value) ++
        mergeSiblingsAsArrRight(rights)

      case _ => throw new RuntimeException("impossible case")
    }

  def objTailToStream(elements: Seq[(String, JsValue)]) = {
    def loop(elements: Seq[(String, JsValue)]): Siblings = elements match {
      case Nil => Stream.empty
      case h :: tail => Node(h._1, h._2) #:: loop(tail)
    }

    loop(elements)
  }

  def arrayTailToStream(elements: Seq[JsValue]) = {
    def loop(elements: Seq[JsValue]): Siblings = elements match {
      case Nil => Stream.empty
      case h :: tail => Node(h) #:: loop(tail)
    }
    loop(elements)
  }

  def localPath(currentPath: JsPath, node: Node, parent: Node, lefts: Siblings): Option[JsPath] = {
    node match {
      case KeyNode(key, _) => Some(currentPath \ key)
      case PlainNode(_)    => parent match {
              case Node.Empty  => Some(currentPath)
              case _           => parent.value match {
                    case JsArray(_) => Some(currentPath(lefts.length))
                    case _          => sys.error("impossible case")
                  }
            }
      case Node.Empty      => None
      case Node.Error(_)   => None
    }
  }

  def build(init: JsZipper)(pathValues: Seq[(JsPath, JsValue)]): JsZipper = init.createOrUpdate(pathValues)

  def buildJsObject(pathValues: Seq[(JsPath, JsValue)]): JsZipper =
    build(JsZipper(Json.obj()))(pathValues)

  def buildJsObject(pathValue: (JsPath, JsValue), pathValues: (JsPath, JsValue)*): JsZipper =
    buildJsObject(Seq(pathValue) ++ pathValues)

  def buildJsArray(pathValues: Seq[(JsPath, JsValue)]): JsZipper =
    build(JsZipper(Json.arr()))(pathValues)

  def buildJsArray(pathValue: (JsPath, JsValue), pathValues: (JsPath, JsValue)*): JsZipper =
    buildJsArray(Seq(pathValue) ++ pathValues)

}


object JsPathExtension{
  def hasKey(path: JsPath): Option[String] = {
    if(path.path.isEmpty) None
    else path.path.last match {
      case KeyPathNode(key) => Some(key)
      case _                => None
    }
  }

  def hasIdx(path: JsPath): Option[Int] = {
    if(path.path.isEmpty) None
    else path.path.last match {
      case IdxPathNode(idx) => Some(idx)
      case _                => None
    }
  }
}
