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
package monad

import scala.util.{Try, Success, Failure}
import scala.annotation.tailrec

import play.api.libs.functional._

trait Monad[M[_]] extends Applicative[M] {
  def bind[A, B](ma: M[A], f: A => M[B]): M[B] 
}

object Monad {
  implicit def apply[M[_]](implicit monad: Monad[M]) = monad
}

package object syntax {
  implicit class MonadOps[M[_], A](ma: M[A])(implicit monad: Monad[M]) {
    def flatMap[B](f: A => M[B]): M[B] = monad.bind(ma, f)
    def map[B](f: A => B): M[B] = monad.map(ma, f)
  }

  implicit val optionMonad = new Monad[Option] {
    def pure[A](a: A): Option[A] = Option(a)
    def map[A, B](ma: Option[A], f: A => B): Option[B] = ma map f
    def apply[A, B](mf: Option[A => B], ma: Option[A]): Option[B] = mf flatMap { f => ma map f }
    def bind[A, B](ma: Option[A], f: A => Option[B]): Option[B] = ma flatMap f
  }

  import scala.concurrent._
  implicit def futureMonad(implicit ctx: ExecutionContext) = new Monad[Future] {
    def pure[A](a: A): Future[A] = Future(a)
    def map[A, B](ma: Future[A], f: A => B): Future[B] = ma map f
    def apply[A, B](mf: Future[A => B], ma: Future[A]): Future[B] = mf flatMap { f => ma map f }
    def bind[A, B](ma: Future[A], f: A => Future[B]): Future[B] = ma flatMap f
  }
}

sealed trait JsZipperM[M[_]] extends JsZipper {
  import JsZipper._
  import JsZipperM._
  import syntax._

  implicit def monad: Monad[M]

  override def toString =
    s"""JsZipperM(focus=$focus, lefts=$lefts, rights=$rights, parents=$parents)"""

  override def left: JsZipperM[M]        = JsZipperM[M](super.left)
  override def down: JsZipperM[M]        = JsZipperM[M](super.down)
  override def right: JsZipperM[M]       = JsZipperM[M](super.right)
  override def up: JsZipperM[M]          = JsZipperM[M](super.up)  
  override def root: JsZipperM[M]        = JsZipperM[M](super.root)
  override def first: JsZipperM[M]       = JsZipperM[M](super.first)
  override def last: JsZipperM[M]        = JsZipperM[M](super.last)
  override def bottomLeft: JsZipperM[M]  = JsZipperM[M](super.bottomLeft)
  override def bottomRight: JsZipperM[M] = JsZipperM[M](super.bottomRight)
  override def orphanize: JsZipperM[M]   = JsZipperM[M](super.orphanize)

  def update(mjs: M[JsValue]): M[JsZipperM[M]] = updateNode( (_:Node) => mjs map { js => Node.copy(focus, js) } )
  def update(fn: JsValue => M[JsValue]): M[JsZipperM[M]] = updateNode( (node:Node) => fn(focus.value) map { js => Node.copy(focus, js) } )
  def updatePathNode(fn: (JsPath, Node) => M[Node]): M[JsZipperM[M]] = updateNode( (n:Node) => fn(path, n) )
  def updateNode(fn: Node => M[Node]): M[JsZipperM[M]] = fn(focus) map { node => 
    (focus, node) match {
      case (KeyNode(_, _), KeyNode(k2, v)) => 
        JsZipperM[M](KeyNode(k2, v), lefts, rights, parents)
      case (PlainNode(_), PlainNode(v)) => 
        JsZipperM[M](PlainNode(v), lefts, rights, parents)
      case (_, Node.Empty) => this // unchanged
      case _ => JsZipperM.Error[M](path -> "Can't update node (key+value or value) to another type of node")
    }
  }
  
  def insertValueLeft(mjs: M[JsValue]): M[JsZipperM[M]] = insertLeftNode( (_: Node) => mjs map { js => Node.copy(focus, js) } )
  def insertValueLeft(fn: JsValue => M[JsValue]): M[JsZipperM[M]] = insertLeftNode( (_: Node) => fn(focus.value) map { js => Node.copy(focus, js) } )
  def insertLeftPathNode(fn: (JsPath, Node) => M[Node]): M[JsZipperM[M]] = insertLeftNode( (n:Node) => fn(path, n) )
  def insertLeftNode(fn: Node => M[Node]): M[JsZipperM[M]] = parent match {
    case Node(parent) => parent match {
      case obj: JsObject => 
        fn(focus) map { _ match {
          case KeyNode(key, value) => 
            JsZipperM[M](
              focus,
              Node(key, value) #:: lefts, 
              rights, 
              parents
            )
          case Node.Empty => this // unchanged
          case _ => JsZipperM.Error[M](path -> "can't add a value to JsObject, expects KeyNode(String, JsValue)")
        } }
              
      case arr: JsArray  => 
        fn(focus) map { _ match {
          case PlainNode(v) => 
            JsZipperM[M](
              focus, 
              Node(v) #:: lefts, 
              rights, 
              parents
            )
          case Node.Empty => this // unchanged
          case _ => JsZipperM.Error[M](path -> "can't add a (key, value) to JsArray, expects PlainNode(JsValue)")
        } }

      case _ => sys.error("Can't have multiple JsValues on root")
    }

    case Node.Empty => monad.pure(JsZipperM.Error[M](path -> "Can't have multiple JsValues on root"))
  } 

  def insertValueRight(mjs: M[JsValue]): M[JsZipperM[M]] = insertRightNode( (_: Node) => mjs map { js => Node.copy(focus, js) } )
  def insertValueRight(fn: JsValue => M[JsValue]): M[JsZipperM[M]] = insertRightNode( (_: Node) => fn(focus.value) map { js => Node.copy(focus, js) } )
  def insertRightPathNode(fn: (JsPath, Node) => M[Node]): M[JsZipperM[M]] = insertRightNode( (n:Node) => fn(path, n) )
  def insertKeyValue(mkv: M[(String, JsValue)]): M[JsZipperM[M]] = insertRightNode( (_: Node) => mkv map { kv => Node(kv._1, kv._2) } )
  def insertKeyValue(fn: (String, JsValue) => M[(String, JsValue)]): M[JsZipperM[M]] = 
    insertRightNode{ (n: Node) => 
      n match {
        case KeyNode(key, value) => fn(key, value) map { case (k, v) => Node(k, v) }
        case _                   => monad.pure(Node.Empty)
      }
    }

  def insertRightNode(fn: Node => M[Node]): M[JsZipperM[M]] = parent match {
    case Node(parent) => parent match {
      case obj: JsObject => 
        fn(focus) map { _ match {
          case KeyNode(key, value) => 
            JsZipperM[M](
              focus,
              lefts, 
              Node(key, value) #:: rights, 
              parents
            )
          case Node.Empty => this // unchanged
          case _ => JsZipperM.Error[M](path -> "can't add a value to JsObject, expects KeyNode(String, JsValue)")
        } }
              
      case arr: JsArray  => 
        fn(focus) map { _ match {
          case PlainNode(value) =>
            JsZipperM[M](
              focus, 
              lefts, 
              Node(value) #:: rights, 
              parents
            )
          case Node.Empty => this // unchanged
          case _ => JsZipperM.Error[M](path -> "can't add a (key, value) to JsArray, expects PlainNode(JsValue)")
        } }

      case _ => sys.error("Can't have multiple JsValues on root")
    }

    case Node.Empty => monad.pure(JsZipperM.Error[M](path -> "Can't have multiple JsValues on root"))
  }

  def insertDown(mnode: M[Node]): M[JsZipperM[M]] = value match {
    case obj: JsObject =>
      if(obj.fields.isEmpty) mnode map { node => node match {
        case KeyNode(key, value) => 
          JsZipperM[M](
            node,
            Stream.Empty,
            Stream.Empty,
            (lefts, focus, rights) #:: parents
          )
        case _ => sys.error("Can't insert/down value in JsObject without key")
      } }
      else down.last.insertRightNode(_ => mnode)

    case arr: JsArray =>
      if(arr.value.isEmpty) mnode map { node => node match {
        case PlainNode(value) => 
          JsZipperM[M](
            node,
            Stream.Empty,
            Stream.Empty,
            (lefts, focus, rights) #:: parents
          )
        case _ => sys.error("Can't insert/down key/value in JsArray")
      } }
      else down.last.insertRightNode(_ => mnode)

    case _ => sys.error("Can't insert/down in value")
  }
  
  override def delete: JsZipperM[M] = JsZipperM[M](super.delete)

  def orElse(other: JsZipperM[M]) = this match {
    case _:JsZipperEmpty => other
    case t               => t
  }  

  override def streamLeft: Stream[JsZipperM[M]]  = 
    this.left match {
      case _: JsZipperEmpty => Stream.Empty
      case zip => zip #:: zip.streamLeft
    }

  override def streamRight: Stream[JsZipperM[M]] = 
    this.right match {
      case _: JsZipperEmpty => Stream.Empty
      case zip => zip #:: zip.streamRight
    }

  /* Vertical streams */
  override def streamDown: Stream[JsZipperM[M]] = 
    this.down match {
      case _: JsZipperEmpty => Stream.Empty
      case zip => zip #:: zip.streamDown
    }

  override def streamUp: Stream[JsZipperM[M]] = 
    this.up match {
      case _: JsZipperEmpty => Stream.Empty
      case zip => zip #:: zip.streamUp
    }

  override def streamDeepRightFocusUp: Stream[JsZipperM[M]] = {
    this.right match {
      case _: JsZipperEmpty => 
        this.up match {
          case _: JsZipperEmpty => Stream(this)
          case lup              => this #:: lup.streamDeepRightFocusUp 
        }
      case rgt => this #:: rgt.streamDeepLeftFocusRightUp
    }
  }
  override def streamDeepRFU: Stream[JsZipperM[M]] = streamDeepRightFocusUp

  override def streamDeepLeftFocusRightUp: Stream[JsZipperM[M]] = {
    if(isLeaf) streamDeepRightFocusUp
    else bottomLeft.streamDeepRightFocusUp
  }
  override def streamDeepLFRU: Stream[JsZipperM[M]] = streamDeepLeftFocusRightUp

  def streamDeepLeftFocusRightUp(filter: JsZipperM[M] => Boolean)(map: JsZipperM[M] => M[JsZipperM[M]]): M[Stream[JsZipperM[M]]] = {
    if(isLeaf) streamDeepRightFocusUp(filter)(map)
    else bottomLeft.streamDeepRightFocusUp(filter)(map)
  }

  /** TODO this stream isn't anymore a real stream 
     as it must evaluates end of stream due to monad */
  def streamDeepRightFocusUp(filter: JsZipperM[M] => Boolean)(map: JsZipperM[M] => M[JsZipperM[M]]): M[Stream[JsZipperM[M]]] = {    
    if(filter(this)) {
      map(this) flatMap { z => z.right match {
        case _:JsZipperEmpty => 
          z.up match {
            case _:JsZipperEmpty => monad.pure(Stream(z))
            case lup             => 
              lup.streamDeepRightFocusUp(filter)(map) map { s => z #:: s }
          }
        case rgt => rgt.streamDeepLeftFocusRightUp(filter)(map) map { s => z #:: s }
      } }
    }
    else {
      this.right match {
        case _:JsZipperEmpty => 
          this.up match {
            case _:JsZipperEmpty => monad.pure(Stream.Empty)
            case lup             => lup.streamDeepRightFocusUp(filter)(map) 
          }
        case rgt => rgt.streamDeepLeftFocusRightUp(filter)(map)
      }
    }
  }

  def find(fn: JsZipperM[M] => Boolean): JsZipperM[M] = 
    streamDeepLFRU.collectFirst{ 
      case zipper if fn(zipper) => zipper
    } getOrElse JsZipperM.Empty[M]()

  def findNext(fn: JsZipperM[M] => Boolean): JsZipperM[M] = 
    // skips this in the stream
    streamDeepRFU.tail.collectFirst{ 
      case zipper if fn(zipper) => zipper
    } getOrElse JsZipperM.Empty[M]()

  override def findByValue(fn: JsValue => Boolean): JsZipperM[M] = findByNode( node => fn(node.value) )
  override def findNextByValue(fn: JsValue => Boolean): JsZipperM[M] = findNextByNode( node => fn(node.value) )

  override def findByNode(fn: Node => Boolean): JsZipperM[M] = 
    streamDeepLFRU.collectFirst{ 
      case zipper if zipper.isPlain && fn(zipper.focus) => zipper
    } getOrElse JsZipperM.Empty[M]()

  override def findNextByNode(fn: Node => Boolean): JsZipperM[M] = 
    // skips this in the stream
    streamDeepRFU.tail.collectFirst{ 
      case zipper if zipper.isPlain && fn(zipper.focus) => zipper
    } getOrElse JsZipperM.Empty[M]()

  override def findByPathNode(fn: (JsPath, Node) => Boolean): JsZipperM[M] = 
    streamDeepLFRU.collectFirst{ 
      case zipper if zipper.isPlain && fn(zipper.path, zipper.focus) => zipper
    } getOrElse JsZipperM.Empty[M]()

  override def findNextByPathNode(fn: (JsPath, Node) => Boolean): JsZipperM[M] = 
    // skips this in the stream
    streamDeepRFU.tail.collectFirst{ 
      case zipper if zipper.isPlain && fn(zipper.path, zipper.focus) => zipper
    } getOrElse JsZipperM.Empty[M]()

  /** same type erasure as super.findAll */
  def findAllM(fn: JsZipperM[M] => Boolean): Stream[JsZipperM[M]] = 
    streamDeepLFRU.collect{
      case zipper if fn(zipper) => zipper
    }

  override def findAllByValue(fn: JsValue => Boolean): Stream[JsZipperM[M]] = 
    streamDeepLFRU.collect{
      case zipper if zipper.isPlain && fn(zipper.focus.value) => zipper
    }

  override def findAllByPathValue(fn: (JsPath, JsValue) => Boolean): Stream[JsZipperM[M]] = 
    streamDeepLFRU.collect{
      case zipper if zipper.isPlain && fn(zipper.path, zipper.focus.value) => zipper
    }

  override def findPath(path: JsPath): JsZipperM[M] = {
    @tailrec
    def step(currentPath: List[PathNode], currentZipper: JsZipperM[M]): JsZipperM[M] = currentPath match {
      case Nil       => currentZipper.up
      case List(p)   => p match {
        case KeyPathNode(pkey) => 
          (currentZipper #:: currentZipper.streamRight).collectFirst{ zipper => zipper.focus match {
            case KeyNode(key, _) if(key == pkey) => zipper
          } } match {
            case None         => JsZipperM.Empty[M]() // not found
            case Some(zipper) => zipper
          }
        case IdxPathNode(idx) =>
          (currentZipper #:: currentZipper.streamRight).drop(idx) match {
            case Stream.Empty  => JsZipperM.Empty[M]() // not found
            case head #:: _    => head
          }
        case _ => JsZipperM.Empty[M]() // TODO (recursive path???)
      }
      case p :: tail => 
        p match {
          case KeyPathNode(pkey) => 
            (currentZipper #:: currentZipper.streamRight).collectFirst{ zipper => zipper.focus match {
              case KeyNode(key, _) if(key == pkey) => zipper
            } } match {
              case None         => JsZipperM.Empty[M]() // not found
              case Some(zipper) => step(tail, zipper.down)
            }
          case IdxPathNode(idx) =>
            (currentZipper #:: currentZipper.streamRight).drop(idx) match {
              case Stream.Empty  => JsZipperM.Empty[M]() // not found
              case head #:: _    => step(tail, head.down)
            }
          case _ => JsZipperM.Empty[M]() // TODO (recursive path???)
        }
    }
    step(path.path, this.down)
  }

  def createOrUpdatePath(path: JsPath, f: JsValue => M[JsValue]): M[JsZipperM[M]] = {
    def step(currentPath: List[PathNode], currentZipper: JsZipperM[M], parentZipper: JsZipperM[M]): M[JsZipperM[M]] = currentPath match {
      case Nil       => currentZipper.up.update(f)
      case List(p)   => p match {
        case KeyPathNode(pkey) => 
          currentZipper match {
            case _:JsZipperEmpty => // inserting in empty object
              parentZipper.insertDown(f(Json.obj()) map { o => KeyNode(pkey, o) })
            case _ => 
              (currentZipper #:: currentZipper.streamRight).collectFirst{ zipper => zipper.focus match {
                case KeyNode(key, _) if(key == pkey) => zipper
              } } match {
                case None => // not found so inserting
                  if(currentZipper.parent.isObject) currentZipper.insertKeyValue(f(Json.obj()) map { o => pkey -> o })
                  else monad.pure(JsZipperM.Empty[M]())
                case Some(zipper) => zipper.update(f(zipper.value))
              }
          }          

        case IdxPathNode(idx) =>
          currentZipper match {
            case _:JsZipperEmpty => // inserting in empty object
              parentZipper.insertDown(f(Json.arr()) map { a => PlainNode(a) })
            case _ => 
              (currentZipper #:: currentZipper.streamRight).drop(idx) match {
                case Stream.Empty | (_:JsZipperEmpty) #:: _ => // not found
                  if(currentZipper.parent.isArray) currentZipper.last.insertValueRight(f(Json.arr()))
                  else monad.pure(JsZipperM.Empty[M]())
                case head #:: _    => head.update(f)
              }
          }
        case _ => monad.pure(JsZipperM.Empty[M]()) // TODO (recursive path???)
      }
      case p :: next :: tail => 
        p match {
          case KeyPathNode(pkey) => 
            (currentZipper #:: currentZipper.streamRight).collectFirst{ zipper => zipper.focus match {
              case KeyNode(key, _) if(key == pkey) => zipper
            } } match {
              case None => // not found
                if(parentZipper.focus.isObject){
                  val n: JsValue = next match {
                    case _:KeyPathNode => Json.obj()
                    case _:IdxPathNode => Json.arr()
                    case _ => JsNull
                  }
                  if(parentZipper.focus.isEmptyObjArr){
                    val z = parentZipper.insertDown(monad.pure(KeyNode(pkey, n): Node))
                    z flatMap { z => step(next :: tail, z.down, z) }
                  }
                  else {
                    val z = currentZipper.last.insertKeyValue( monad.pure(pkey -> n) )
                    z flatMap { z => 
                      val zr = z.right
                      step(next :: tail, zr.down, zr)
                    }
                  }
                }
                else monad.pure(JsZipperM.Empty[M]())
              case Some(zipper) => step(next :: tail, zipper.down, zipper)
            }
          case IdxPathNode(idx) =>
            (currentZipper #:: currentZipper.streamRight).drop(idx) match {
              case Stream.Empty | (_:JsZipperEmpty) #:: _ => // not found
                if(parentZipper.focus.isArray) {
                  val n: JsValue = next match {
                    case _:KeyPathNode => Json.obj()
                    case _:IdxPathNode => Json.arr()
                    case _             => JsNull
                  }
                  if(parentZipper.focus.isEmptyObjArr){
                    val z = parentZipper.insertDown(monad.pure(PlainNode(n): Node))
                    z flatMap { z => step(next :: tail, z.down, z) }
                  } else {
                    val z = currentZipper.last.insertValueRight(monad.pure(n))
                    z flatMap { z => 
                      val zr = z.right
                      step(next :: tail, zr.down, zr)
                    }
                  }
                }
                else monad.pure(JsZipperM.Empty[M]())
              case head #:: _    => step(next :: tail, head.down, head)
            }
          case _ => monad.pure(JsZipperM.Empty[M]()) // TODO (recursive path???)
        }
    }
    step(path.path, this.down, this) map { z => z.root }
  }
    
  def createOrUpdatePath(path: JsPath, mjs: M[JsValue]): M[JsZipperM[M]] = createOrUpdatePath(path, _ => mjs)

  def createOrUpdate(pathValues: Seq[(JsPath, M[JsValue])]): M[JsZipperM[M]] = {
    def step(pathValues: List[(JsPath, M[JsValue])], zipper: JsZipperM[M]): M[JsZipperM[M]] = {
      pathValues match {
        case Nil          => monad.pure(JsZipperM.Empty[M]())
        case List(pv)     => zipper.createOrUpdatePath(pv._1, pv._2)
        case head :: tail => zipper.createOrUpdatePath(head._1, head._2) flatMap { zipper => step(tail, zipper) }
      }
    }
    
    step(pathValues.toList, this)
  }  

  def createOrUpdate(pathValue: (JsPath, M[JsValue]), pathValues: (JsPath, M[JsValue])*): M[JsZipperM[M]] =
    createOrUpdate(Seq(pathValue) ++ pathValues)

  def filterMapThrough(filterFn: JsZipperM[M] => Boolean)(mapFn: JsZipperM[M] => M[JsZipperM[M]]): M[JsZipperM[M]] = {
    def step(zipper: JsZipperM[M]): M[JsZipperM[M]] = {      
      zipper match {
        case _:JsZipperEmpty => monad.pure(JsZipperM.Empty[M]())
        case found           => 
          mapFn(found) flatMap { updated =>
            updated.findNext(filterFn) match {
              case _:JsZipperEmpty  => monad.pure(updated)
              case found            => step(found)
            }
          }
      } 
    }

    step(find(filterFn))
  }    
}

object JsZipperM {

  import JsZipper._

  def apply[M[_]](theZipper: JsZipper)(implicit m: Monad[M]) =
    theZipper match {
      case JsZipper.Empty    => JsZipperM.Empty[M]()(m)
      case JsZipper.Error(e) => JsZipperM.Error[M](e)(m)
      case z                 =>
        new JsZipperM[M] {
          implicit val monad: Monad[M] = m
          override val focus   = z.focus
          override val lefts   = z.lefts
          override val rights  = z.rights
          override val parents = z.parents
        }
    }
    
  def apply[M[_]](js: JsValue)(implicit m: Monad[M]): JsZipperM[M] = apply[M](JsZipper(js))(m)

  def apply[M[_]](theFocus: Node, theLefts: Siblings, theRights: Siblings, theParents: Parents)(implicit m: Monad[M]): JsZipperM[M] =
    apply[M](JsZipper(theFocus, theLefts, theRights, theParents))(m)
  
  case class Empty[M[_]]()(implicit m: Monad[M]) extends JsZipperEmpty with JsZipperM[M] {
    implicit val monad: Monad[M] = m

    /* enables JsEmptyM == JsEmpty */
    override def equals(that: Any): Boolean = 
      that.isInstanceOf[JsZipperEmpty] && this.hashCode == that.asInstanceOf[JsZipperEmpty].hashCode

    override def hashCode = JsZipper.Empty.hashCode

    s"""JsZipperM.Empty"""
  }

  case class Error[M[_]](theError: (JsPath, String))(implicit m: Monad[M]) extends JsZipperError with JsZipperM[M] { 
    implicit val monad: Monad[M] = m
    override val error = theError

    override def toString = 
      s"""JsZipperM.Error(error: $error, focus=$focus, lefts=$lefts, rights=$rights, parents=$parents)"""
  }

  def build[M[_]: Monad](init: JsZipperM[M])(pathValues: Seq[(JsPath, M[JsValue])]): M[JsZipperM[M]] = init.createOrUpdate(pathValues)

  def buildJsObject[M[_]: Monad](pathValues: Seq[(JsPath, M[JsValue])]): M[JsZipperM[M]] =
    build(JsZipperM[M](Json.obj()))(pathValues)

  def buildJsObject[M[_]: Monad](pathValue: (JsPath, M[JsValue]), pathValues: (JsPath, M[JsValue])*): M[JsZipperM[M]] =
    buildJsObject(Seq(pathValue) ++ pathValues)

  def buildJsArray[M[_]: Monad](pathValues: Seq[(JsPath, M[JsValue])]): M[JsZipperM[M]] =
    build(JsZipperM[M](Json.arr()))(pathValues)

  def buildJsArray[M[_]: Monad](pathValue: (JsPath, M[JsValue]), pathValues: (JsPath, M[JsValue])*): M[JsZipperM[M]] =
    buildJsArray(Seq(pathValue) ++ pathValues)

}
