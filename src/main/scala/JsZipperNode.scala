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

sealed trait Node {
  def value: JsValue

  def filter(fn: JsValue => Boolean) = if(fn(value)) this else Node.empty

  def isArray = value match {
    case _: JsArray => true
    case _ => false
  }

  def isObject = value match {
    case _: JsObject => true
    case _ => false
  }

  def isEmptyObjArr = value match {
    case JsObject(fields) if(fields.isEmpty) => true
    case JsArray(value) if (value.isEmpty) => true
    case _ => false
  }

  def isValue = value match {
    case _: JsObject => false
    case _: JsArray => false
    case _ => false
  }

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

  def copyKeyNode(node: Node, newKeyValue: (String, JsValue)) = node match {
    case KeyNode(_, _)    => KeyNode(newKeyValue._1, newKeyValue._2)
    case _                => node
  }
}

case class KeyNode(val key: String, override val value: JsValue) extends Node
case class PlainNode(override val value: JsValue) extends Node
