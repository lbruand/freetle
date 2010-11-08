 /*
  * Copyright 2010 Lucas Bruand
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
 
package org.freetle.util

import scala.xml.{MetaData, NamespaceBinding}
import javax.xml.stream.Location
import javax.xml.namespace.NamespaceContext
import javax.xml.XMLConstants

/** This class represents an XML event for pull parsing.
 *  Pull parsing means that during the traversal of the XML
 *  tree we are parsing, each "event" is returned to the caller
 *  and the traversal is suspended.
 */
sealed abstract class XMLEvent {
  var location : Location = null
  var namespaceContext : NamespaceContext = null
  override def toString() : String = toStream.mkString
  def toStream : Stream[Char] = toString.toStream

}

case class QName(namespaceURI : String = XMLConstants.NULL_NS_URI,
                 localPart: String,
                 prefix : String = XMLConstants.DEFAULT_NS_PREFIX) {

}
/** An element is encountered the first time */
case class EvElemStart(name : QName, attributes : Map[QName, String]) extends XMLEvent {
  private final def buildAttrStringBuffer(sb :StringBuilder)(j : Tuple2[QName, String]): Unit = {
    sb.append(' ')
    if (!j._1.prefix.isEmpty) {
      sb.append(j._1.prefix)
      sb.append(':')
    }
    sb.append(j._1.localPart)
    sb.append('=')
    sb.append('"')
    sb.append(j._2)
    sb.append('"')
  }
  override final def toStream() :  Stream[Char] = {
    var sb = new StringBuilder()
    sb.append('<')
    if (!name.prefix.isEmpty) {
      sb.append(name.prefix)
      sb.append(':')
    }
    sb.append(name.localPart)

    // Attributes
    if (attributes != null) {
      attributes.foreach(
         buildAttrStringBuffer(sb)
      )
    }
    sb.append('>')
    sb.toStream
  }
}

/** An element is encountered the last time */
case class EvElemEnd(name : QName) extends XMLEvent {
  override final def toStream() :  Stream[Char] = {
    var sb = new StringBuilder()
    sb.append('<')
    sb.append('/')
    if (!name.prefix.isEmpty) {
      sb.append(name.prefix)
      sb.append(':')
    }
    sb.append(name.localPart)
    sb.append('>')
    sb.toStream
  }
}
/** A text node is encountered */
case class EvText(text : String) extends XMLEvent {
  override final def toStream() :  Stream[Char] = text.toStream
}

/** An entity reference is encountered */
case class EvEntityRef(entity: String) extends XMLEvent {
  override final def toStream() :  Stream[Char] = {
    var sb = new StringBuilder()
    sb.append('&')
    sb.append(entity)
    sb.append(';')
    sb.toStream
  }
}

/** A processing instruction is encountered */
case class EvProcInstr(target: String, text: String) extends XMLEvent {
  override final def toStream() : Stream[Char] = Stream.empty
}

/** A comment is encountered */
case class EvComment(text: String) extends XMLEvent {
  override final def toStream() : Stream[Char] =
    (new StringBuilder()).append("<!-- ").append(text).append(" -->").toStream
}

/** Used when we want a empty but yet positive result */
case class EvPositiveResult() extends XMLEvent {
  override final def toStream() : Stream[Char] = toString.toStream
  override final def toString() = "<!-- EvPositiveResult -->"
}



abstract class XMLEventMatcher extends (XMLEvent => Boolean)

class NeverMatcher extends XMLEventMatcher {
  def apply(event : XMLEvent) : Boolean = false
}

class AlwaysMatcher extends XMLEventMatcher {
  def apply(event : XMLEvent) : Boolean = true
}

/**
 * This can not be used directly because of type erasure.
 */
trait TypeMatcher[+ParamEvent <: XMLEvent] extends XMLEventMatcher {
  def apply(event : XMLEvent) : Boolean = event.isInstanceOf[ParamEvent]
}
class EvElemStartTypeMatcher extends TypeMatcher[EvElemStart] {
  override def apply(event : XMLEvent) : Boolean = event.isInstanceOf[EvElemStart]
}
class EvElemEndTypeMatcher extends TypeMatcher[EvElemEnd] {
  override def apply(event : XMLEvent) : Boolean = event.isInstanceOf[EvElemEnd]
}
class EvTextTypeMatcher extends TypeMatcher[EvText] {
  override def apply(event : XMLEvent) : Boolean = event.isInstanceOf[EvText]
}
class EvEntityRefTypeMatcher extends TypeMatcher[EvEntityRef] {
  override def apply(event : XMLEvent) : Boolean = event.isInstanceOf[EvEntityRef]
}
class EvProcInstrTypeMatcher extends TypeMatcher[EvProcInstr] {
  override def apply(event : XMLEvent) : Boolean = event.isInstanceOf[EvProcInstr]
}
class EvCommentTypeMatcher extends TypeMatcher[EvComment] {
  override def apply(event : XMLEvent) : Boolean = event.isInstanceOf[EvComment]
}

abstract class FilterMatcher[+ParamEvent <: XMLEvent] extends TypeMatcher[ParamEvent] {
  def apply[ParamEvent](event : ParamEvent) : Boolean
  override def apply(event : XMLEvent) : Boolean = event match {
    case ev : ParamEvent => apply[ParamEvent](ev)
    case _ => false
  }
}

class OrMatcher(t1 : XMLEventMatcher, t2 : XMLEventMatcher) extends XMLEventMatcher {
  def apply(event : XMLEvent) : Boolean = t1(event) || t2(event)
}

class AndMatcher(t1 : XMLEventMatcher, t2 : XMLEventMatcher) extends XMLEventMatcher {
  def apply(event : XMLEvent) : Boolean = t1(event) && t2(event)
}

abstract class FilterTextMatcher extends FilterMatcher[EvText] {
  def filter : (String => Boolean)
  def apply[EvText](event : EvText) : Boolean = event match { // Very inelegant but filter(event.text) does not seem to work.
    case EvText(text : String) => filter(text)
    case _ => false
  }
}
class SpaceMatcher extends FilterTextMatcher {
  final def filter =  x => "".equals(x.trim())
}

class SpaceOrCommentMatcher extends OrMatcher(new SpaceMatcher(), new EvCommentTypeMatcher())

