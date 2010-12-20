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

import javax.xml.stream.Location
import javax.xml.namespace.NamespaceContext
import javax.xml.XMLConstants
import java.io.{StringWriter, Writer}

/** This class represents an XML event for pull parsing.
 *  Pull parsing means that during the traversal of the XML
 *  tree we are parsing, each "event" is returned to the caller
 *  and the traversal is suspended.
 */
sealed abstract class XMLEvent {
  var location : Location = null
  var namespaceContext : NamespaceContext = null
  override def toString() : String = toStream.mkString

  final def toStream() :  Stream[Char] = {
    var sb = new StringWriter()
    appendWriter(sb)
    sb.toString.toStream
  }
  
  def appendWriter(writer : Writer) : Unit
}

case class QName(namespaceURI : String = XMLConstants.NULL_NS_URI,
                 localPart: String,
                 prefix : String = XMLConstants.DEFAULT_NS_PREFIX) {

}
/** An element is encountered the first time */
case class EvElemStart(name : QName, attributes : Map[QName, String]) extends XMLEvent {
  private final def buildAttrStringBuffer(sb :Writer)(j : Tuple2[QName, String]): Unit = {
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

  final def appendWriter(sb: Writer): Unit = {
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
  }

}

/** An element is encountered the last time */
case class EvElemEnd(name : QName) extends XMLEvent {
  final def appendWriter(sb: Writer): Unit = {
    sb.append('<')
    sb.append('/')
    if (!name.prefix.isEmpty) {
      sb.append(name.prefix)
      sb.append(':')
    }
    sb.append(name.localPart)
    sb.append('>')
  }
}
/** A text node is encountered */
case class EvText(text : String) extends XMLEvent {
  final def appendWriter(sb: Writer): Unit = {
    sb.append(text)
  }
}

/** An entity reference is encountered */
case class EvEntityRef(entity: String) extends XMLEvent {
  final def appendWriter(sb: Writer): Unit = {
    sb.append('&')
    sb.append(entity)
    sb.append(';')
  }
}

/** A processing instruction is encountered */
case class EvProcInstr(target: String, text: String) extends XMLEvent {
  final def appendWriter(sb: Writer): Unit = {}
}

/** A comment is encountered */
case class EvComment(text: String) extends XMLEvent {
  final def appendWriter(sb: Writer): Unit = {
    sb.append("<!-- ")
    sb.append(text)
    sb.append(" -->")
  }
}

