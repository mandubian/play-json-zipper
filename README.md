> EXPERIMENTAL / DRAFT

# JsZipper : Play2 Json advanced (& monadic) manipulations

> `JsZipper` is a new tool allowing much more complex & powerful manipulations of Json structures for Play2/Json API:

> `JsZipper` is inspired by the [Zipper](http://en.wikipedia.org/wiki/Zipper_\(data_structure\)) concept introduced by [Gerard Huet](http://en.wikipedia.org/wiki/G%C3%A9rard_Huet) in 1997. 

>The Zipper allows to update immutable traversable structures in an efficient way. Json is an immutable AST so it fits well. FYI, the Zipper behaves like a loupe that walks through each node of the AST (left/right/up/down) while keeping aware of the nodes on its left, its right and its upper. The interesting idea behind the loupe is that when it targets a node, it can modify and even delete the focused node. The analogy to the pants zipper is quite good too because when it goes down the tree, it behaves as if it was <i>opening</i> the tree to be able to drive the loupe through all nodes and when it goes up, it <i>closes</i> back the tree... I won't tell more here, it would be too long.

> `JsZipper` is a specific interpretation of Zipper concept for Play/Json API based on :
>   - Scala Streams to go through / update / construct Json AST in a lazy way
>   - Monadic aspects to provide _funnier_ ways of manipulating the Json AST (plz see below)

_Please note, `JsZipper` is not an end in itself but a tool useful to provide new API to manipulate Json._

## Use it in your SBT project

> I've just published v1.1 of JsZipper to Bintray instead of my fake maven repo in github.
> It's much more reliable.

```scala
resolvers += "mandubian maven bintray" at "http://dl.bintray.com/mandubian/maven"

libraryDependencies ++= Seq(
  "com.mandubian"     %% "play-json-zipper"    % "1.2"
)
```

## Samples

Let's go to samples.

We'll use following Json Object.

```scala
scala> val js = Json.obj(
  "key1" -> Json.obj(
    "key11" -> "TO_FIND",
    "key12" -> 123L,
    "key13" -> JsNull
  ),
  "key2" -> 123,
  "key3" -> true,
  "key4" -> Json.arr("TO_FIND", 345.6, "test", Json.obj("key411" -> Json.obj("key4111" -> "TO_FIND")))
)
js: play.api.libs.json.JsObject = {"key1":{"key11":"TO_FIND","key12":123,"key13":null},"key2":123,"key3":true,"key4":["TO_FIND",345.6,"test",{"key411":{"key4111":"TO_FIND"}}]}
```

# Basic manipulations

## Setting multiple paths/values 

```scala
scala> js.set(
  (__ \ "key4")(2) -> JsNumber(765.23),
  (__ \ "key1" \ "key12") -> JsString("toto")
)
res1: play.api.libs.json.JsValue = {"key1":{"key11":"TO_FIND","key12":"toto","key13":null},"key2":123,"key3":true,"key4":["TO_FIND",345.6,765.23,{"key411":{"key4111":"TO_FIND"}}]}
```

## Deleting multiple paths/values

```scala
scala> js.delete(
  (__ \ "key4")(2),
  (__ \ "key1" \ "key12"),
  (__ \ "key1" \ "key13")
)
res2: play.api.libs.json.JsValue = {"key1":{"key11":"TO_FIND"},"key2":123,"key3":true,"key4":["TO_FIND",345.6,{"key411":{"key4111":"TO_FIND"}}]}
```

## Finding paths/values according to a filter

```scala
scala> js.findAll( _ == JsString("TO_FIND") ).toList
res5: List[(play.api.libs.json.JsPath, play.api.libs.json.JsValue)] = List(
  (/key1/key11,"TO_FIND"), 
  (/key4(0),"TO_FIND"), 
  (/key4(3)/key411/key4111,"TO_FIND")
)
```

## Updating values according to a filter based on value

```scala
scala> js.updateAll( (_:JsValue) == JsString("TO_FIND") ){ js =>
  val JsString(str) = js
  JsString(str + "2")
}
res6: play.api.libs.json.JsValue = {"key1":{"key11":"TO_FIND2","key12":123,"key13":null},"key2":123,"key3":true,"key4":["TO_FIND2",345.6,"test",{"key411":{"key4111":"TO_FIND2"}}]}
```

## Updating values according to a filter based on path+value

```scala
scala> js.updateAll{ (path, js) =>
  JsPathExtension.hasKey(path) == Some("key4111")
}{ (path, js) =>
  val JsString(str) = js
  JsString(str + path.path.last)
}
res1: play.api.libs.json.JsValue = {"key1":{"key11":"TO_FIND","key12":123,"key13":null},"key2":123,"key3":true,"key4":["TO_FIND",345.6,"test",{"key411":{"key4111":"TO_FIND/key4111"}}]}
```

## Creating an object from scratch

```scala
scala> val build = JsExtensions.buildJsObject( 
  __ \ "key1" \ "key11" -> JsString("toto"),
  __ \ "key1" \ "key12" -> JsNumber(123L),
  (__ \ "key2")(0)      -> JsBoolean(true),
  __ \ "key3"           -> Json.arr(1, 2, 3)
)
build: play.api.libs.json.JsValue = {"key1":{"key11":"toto","key12":123},"key3":[1,2,3],"key2":[true]}
```

<br/>
# Let's be funnier with Monads now

> Let's use `Future` as our Monad because it's... coooool to do things in the future ;)

Imagine you call several services returning `Future[JsValue]` and you want to build/update a `JsObject` from it. 
Until now, if you wanted to do that with Play2/Json, it was quite tricky and required some code.

Here is what you can do now.

## Updating multiple _FUTURE_ values at given paths

```scala
scala> val maybeJs = js.setM[Future](
  (__ \ "key4")(2)        -> future{ JsNumber(765.23) },
  (__ \ "key1" \ "key12") -> future{ JsString("toto") }
)
maybeJs: scala.concurrent.Future[play.api.libs.json.JsValue] = scala.concurrent.impl.Promise$DefaultPromise@6beb722d

scala> Await.result(maybeJs, Duration("2 seconds"))
res4: play.api.libs.json.JsValue = {"key1":{"key11":"TO_FIND","key12":"toto","key13":null},"key2":123,"key3":true,"key4":["TO_FIND",345.6,765.23,{"key411":{"key4111":"TO_FIND"}}]}
```

## Update multiple _FUTURE_ values according to a filter

```scala
scala> val maybeJs = js.updateAllM[Future]( (_:JsValue) == JsString("TO_FIND") ){ js =>
  future {
    val JsString(str) = js
    JsString(str + "2")
  }
}
maybeJs: scala.concurrent.Future[play.api.libs.json.JsValue] = scala.concurrent.impl.Promise$DefaultPromise@35a4bb1a

scala> Await.result(maybeJs, Duration("2 seconds"))
res6: play.api.libs.json.JsValue = {"key1":{"key11":"TO_FIND2","key12":123,"key13":null},"key2":123,"key3":true,"key4":["TO_FIND2",345.6,"test",{"key411":{"key4111":"TO_FIND2"}}]}
```

## Creating a _FUTURE_ JsArray from scratch

```scala
scala> val maybeArr = JsExtensions.buildJsArrayM[Future](
  future { JsNumber(123.45) },
  future { JsString("toto") }
)
maybeArr: scala.concurrent.Future[play.api.libs.json.JsValue] = scala.concurrent.impl.Promise$DefaultPromise@220d48e4

scala> Await.result(maybeArr, Duration("2 seconds"))
res0: play.api.libs.json.JsValue = [123.45,"toto"]
```

Ok, much more can be done...
Have fun!
