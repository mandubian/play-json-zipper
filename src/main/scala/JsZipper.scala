package play.api.libs.json

import scala.util.{Try, Success, Failure}
import scala.annotation.tailrec

sealed trait Node {
  def value: JsValue

  def filter(fn: JsValue => Boolean) = if(fn(value)) this else Node.empty
}

object Node {
  val empty = Node.Empty

  case object Empty extends Node {
    override val value = JsUndefined("undef")
  }

  case class Error(error: (JsPath, String)) extends Node {
    override val value = JsUndefined("error")
  }

  def apply(key: String, value: JsValue): Node = KeyNode(key, value)
  def apply(value: JsValue): Node = PlainNode(value)

  def unapply(node: Node): Option[JsValue] = Some(node.value)

  def copy(node: Node) = node match {
    case Node.Empty           => Node.Empty
    case KeyNode(key, value)  => KeyNode(key, value)
    case PlainNode(value)     => PlainNode(value)
    case Error(e)             => Error(e)
  }

  def copy(node: Node, newValue: JsValue) = node match {
    case Node.Empty        => Node.Empty
    case KeyNode(key, _)  => KeyNode(key, newValue)
    case PlainNode(_)     => PlainNode(newValue)
    case Error(e)             => Error(e)
  }
}

case class KeyNode(val key: String, override val value: JsValue) extends Node
case class PlainNode(override val value: JsValue) extends Node

sealed trait JsZipper {
  import JsZipper._

  def focus:   Node
  def lefts:   Siblings
  def rights:  Siblings
  def parents: Parents

  def value = focus.value

  def isPlain = this match {
    case JsZipper.Empty => false
    case JsZipper.Error(_) => false
    case _ => true
  }

  def isEmpty = this match {
    case JsZipper.Empty => true
    case _ => false
  }

  def isError = this match {
    case JsZipper.Error(_) => true
    case _ => false
  }

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
    case Stream.Empty => JsZipper.Empty
  }

  def right: JsZipper = rights match {
    case head #:: tail   => JsZipper(head, focus #:: lefts, tail, parents)
    case Stream.Empty => JsZipper.Empty
  }

  def up: JsZipper = parents match {
    case (plefts, parent, prights) #:: ancestors => 
      val js = reify(focus, lefts, rights, parent)
      JsZipper(
        Node.copy(parent, js), 
        plefts, prights, ancestors
      )
    case Stream.Empty => JsZipper.Empty
  }

  def first: JsZipper = {
    def loop(zip: JsZipper): JsZipper = zip.left match {
      case JsZipper.Empty => zip
      case lft => loop(zip.left)
    }
    loop(this)
  }

  def last: JsZipper = {
    def loop(zip: JsZipper): JsZipper = zip.right match {
      case JsZipper.Empty => zip
      case lft => loop(zip.right)
    }
    loop(this)
  }

  def root: JsZipper = this.up match {
    case JsZipper.Empty    => this
    case e: JsZipper.Error => e
    case parent            => parent.root
  }

  def bottomLeft: JsZipper = {
    def loop(zip: JsZipper): JsZipper = zip.down match {
      case JsZipper.Empty => zip
      case dwn => loop(dwn)
    }
    loop(this)
  }

  def bottomRight: JsZipper = {
    def loop(zip: JsZipper): JsZipper = zip.last.down match {
      case JsZipper.Empty => zip
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
  def updateNode(fn: Node => Node): JsZipper = (focus, fn(focus)) match {
    case (KeyNode(_, _), KeyNode(k2, v)) => 
      JsZipper(KeyNode(k2, v), lefts, rights, parents)
    case (PlainNode(_), PlainNode(v)) => 
      JsZipper(PlainNode(v), lefts, rights, parents)
    case (_, Node.Empty) => this // unchanged
    case _ => JsZipper.Error(path -> "Can't update node (key+value or value) to another type of node")
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
          val pnode = parent match {
            case KeyNode(key, _) => Node(key, Json.obj())
            case PlainNode(_)    => Node(Json.arr())
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

  // def filterByValue(fn: JsValue => Boolean): JsZipper = 
  //   if(this.isPlain && fn(this.focus.value)) this
  //   else JsZipper.Empty

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
  def streamDeeRFU: Stream[JsZipper] = streamDeepRightFocusUp

  def streamDeepLeftFocusRightUp: Stream[JsZipper] = {
    if(isLeaf) streamDeepRightFocusUp
    else bottomLeft.streamDeepRightFocusUp
  }
  def streamDeepLFRU: Stream[JsZipper] = streamDeepLeftFocusRightUp

  def streamDeepRightFocusUp(preLoop: JsZipper => JsZipper): Stream[JsZipper] = {
    val z = preLoop(this)
    z.right match {
      case JsZipper.Empty => 
        z.up match {
          case JsZipper.Empty => Stream(z)
          case lup            => z #:: lup.streamDeepRightFocusUp 
        }
      case rgt => z #:: rgt.streamDeepLeftFocusRightUp
    }
  }
  
  /* In width streams */
  def streamWideFocusRightDown: Stream[JsZipper] = {
    this match {
      case JsZipper.Empty => Stream.Empty
      case zip            => 
        val str = zip #:: zip.streamRight
        str ++ str.flatMap( _.down.streamWideFocusRightDown )
    }
  }
  def streamWideFRD: Stream[JsZipper] = streamWideFocusRightDown

  def findIncluded(fn: JsZipper => Boolean): JsZipper = 
    streamDeepLFRU.collectFirst{ 
      case zipper if fn(zipper) => zipper
    } getOrElse JsZipper.Empty

  def findExcluded(fn: JsZipper => Boolean): JsZipper = 
    // skips this in the stream
    streamDeeRFU.tail.collectFirst{ 
      case zipper if fn(zipper) => zipper
    } getOrElse JsZipper.Empty

  def findValueIncluded(fn: JsValue => Boolean): JsZipper = findNodeIncluded( node => fn(node.value) )
  def findValueExcluded(fn: JsValue => Boolean): JsZipper = findNodeExcluded( node => fn(node.value) )

  def findNodeIncluded(fn: Node => Boolean): JsZipper = 
    streamDeepLFRU.collectFirst{ 
      case zipper if zipper.isPlain && fn(zipper.focus) => zipper
    } getOrElse JsZipper.Empty

  def findNodeExcluded(fn: Node => Boolean): JsZipper = 
    // skips this in the stream
    streamDeeRFU.tail.collectFirst{ 
      case zipper if zipper.isPlain && fn(zipper.focus) => zipper
    } getOrElse JsZipper.Empty

  def findPathNodeIncluded(fn: (JsPath, Node) => Boolean): JsZipper = 
    streamDeepLFRU.collectFirst{ 
      case zipper if zipper.isPlain && fn(zipper.path, zipper.focus) => zipper
    } getOrElse JsZipper.Empty

  def findPathNodeNextExcluded(fn: (JsPath, Node) => Boolean): JsZipper = 
    // skips this in the stream
    streamDeeRFU.tail.collectFirst{ 
      case zipper if zipper.isPlain && fn(zipper.path, zipper.focus) => zipper
    } getOrElse JsZipper.Empty

  def findPath(path: JsPath): JsZipper = 
    streamWideFRD.collectFirst{
      case zipper if zipper.isPlain && zipper.path == path => zipper
    } getOrElse JsZipper.Empty

  def findAll(fn: JsValue => Boolean): Stream[JsZipper] = 
    streamDeepLFRU.collect{
      case zipper if zipper.isPlain && fn(zipper.focus.value) => zipper
    }

  def findAllPathValue(fn: (JsPath, JsValue) => Boolean): Stream[JsZipper] = 
    streamDeepLFRU.collect{
      case zipper if zipper.isPlain && fn(zipper.path, zipper.focus.value) => zipper
    }

  def updateAll(findFn: JsValue => Boolean)(updateFn: JsValue => JsValue): JsZipper = {
    @tailrec
    def step(zipper: JsZipper): JsZipper = {      
      zipper match {
        case JsZipper.Empty => JsZipper.Empty
        case found          => 
          val updated = found.update(updateFn)

          updated.findValueExcluded(findFn) match {
            case JsZipper.Empty  => updated
            case found           => step(updated)
          }
      } 
    }

    step(this.findValueIncluded(findFn))
  }

  /*def map(fn: JsZipper => JsZipper): JsZipper = 
  //def filter(fn: JsZipper => Boolean): Stream[JsZipper] = streamDeepLFRU.filter(fn)

  def withFilter(fn: JsZipper => Boolean): JsZipper = {
    
  }*/

  def filterMap(filterFn: JsZipper => Boolean)(mapFn: JsZipper => JsZipper): JsZipper = {
    @tailrec
    def step(zipper: JsZipper): JsZipper = {      
      zipper match {
        case JsZipper.Empty => JsZipper.Empty
        case found          => 
          val updated = mapFn(found)

          updated.findExcluded(filterFn) match {
            case JsZipper.Empty  => updated
            case found           => step(updated)
          }
      } 
    }

    step(this.findIncluded(filterFn))
  }

  def pathValue: (JsPath, JsValue) = (path, focus.value)

  override def toString =
    s"""JsZipper(focus=$focus, lefts=$lefts, rights=$rights, parents=$parents)"""
}

trait JsZipperOps {
  def zipStream(js: JsValue): Stream[JsZipper] = JsZipper(js).streamDeepLFRU
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

  case object Empty extends JsZipper {
    val focus = Node.Empty
    val lefts = Stream.Empty
    val rights = Stream.Empty
    val parents = Stream.Empty

    override def toString = 
      s"""JsZipper.Empty(focus=$focus, lefts=$lefts, rights=$rights, parents=$parents)"""
  }

  case class Error(error: (JsPath, String)) extends JsZipper { 
    override val lefts = Stream.Empty
    override val rights = Stream.Empty
    override val parents = Stream.Empty
    override val focus = Node.Error(error)

    override def toString = 
      s"""JsZipper.Error(error: $error, focus=$focus, lefts=$lefts, rights=$rights, parents=$parents)"""
  }

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
      case KeyNode(key, value) => seq :+ (key -> value) 
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
        mergeSiblingsAsObj(lefts) +   
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

}

object JsProcess {
  import com.clarifi.machines._  
  import Plan._

  def find(f: JsValue => Boolean): Process[JsZipper, JsZipper] = {
    def step(zipper: JsZipper): Process[JsZipper, JsZipper] = {      
      zipper match {
        case JsZipper.Empty => Stop
        case found          => Emit( found, () => step(found.findValueExcluded(f)) )
      }
    }

    await[JsZipper] map(_.findValueIncluded(f)) flatMap(step)
  } 

  def update(findFn: JsValue => Boolean)(updateFn: JsValue => JsValue): Process[JsZipper, JsZipper] = {
    def step(zipper: JsZipper): Process[JsZipper, JsZipper] = {      
      zipper match {
        case JsZipper.Empty => Stop
        case found          => 
          val updated = found.update(updateFn)

          updated.findValueExcluded(findFn) match {
            case JsZipper.Empty  => emit(updated) >> Stop
            case found           => Emit( found, () => step(found) )
          }
      } 
    }

    await[JsZipper] map(_.findValueIncluded(findFn)) flatMap(step)
  }
    
  def last[A]: Process[A,A] = {
    def step[A](a: A): Process[A, A] = {
      await[A] orElse (emit(a) >> Stop) flatMap step
    }

    await[A] flatMap(step(_:A))
  }
}

object JsPathExtension{
  def hasKey(path: JsPath): Option[String] = path.path.last match{
    case KeyPathNode(key) => Some(key)
    case _                => None
  }

  def hasIdx(path: JsPath): Option[Int] = path.path.last match{
    case IdxPathNode(idx) => Some(idx)
    case _                => None
  }
}
