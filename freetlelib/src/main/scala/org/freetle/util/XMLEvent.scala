 /*
  * Copyright 2010-2012 Lucas Bruand
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
import java.io._

/** This class represents an XML event for pull parsing.
 *  Pull parsing means that during the traversal of the XML
 *  tree we are parsing, each "event" is returned to the caller
 *  and the traversal is suspended.
 */
@SerialVersionUID(32001)
sealed abstract class XMLEvent extends Externalizable {
  var location : Location = null
  var namespaceContext : NamespaceContext = null
  override def toString: String = toStream.mkString

  final def toStream:  Stream[Char] = {
    var sb = new StringWriter()
    appendWriter(sb)
    sb.toString.toStream
  }
  
  def appendWriter(writer : Writer)
}

/**
 * This class represents a qualified name.
 */
@SerialVersionUID(32002)
case class QName(var namespaceURI : String = XMLConstants.NULL_NS_URI,
                 var localPart: String = null,
                 var prefix : String = XMLConstants.DEFAULT_NS_PREFIX) extends Externalizable {
  def readExternal(in: ObjectInput) {
    namespaceURI = in.readUTF
    localPart = in.readUTF
    prefix = in.readUTF
  }

  def writeExternal(out: ObjectOutput) {
    out.writeUTF(this.namespaceURI)
    out.writeUTF(this.localPart)
    out.writeUTF(this.prefix)
  }
}

/** An element representing an openning tag */
@SerialVersionUID(32003)
case class EvElemStart(var name : QName = null, var attributes : Map[QName, String] = null, var namespaces : Map[String, String] = null) extends XMLEvent {
  final def this() = this(null, null)
  private final def buildAttrStringBuffer(sb :Writer)(j : (QName, String)) {
    sb.append(' ')
    if (j._1.prefix.length() != 0) {
      sb.append(j._1.prefix)
      sb.append(':')
    }
    sb.append(j._1.localPart)
    sb.append('=')
    sb.append('"')
    sb.append(j._2)
    sb.append('"')
  }

  final def appendWriter(sb: Writer) {
    sb.append('<')
    if (name.prefix.length() != 0) {
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

  def readExternal(in: ObjectInput) {
    this.name = new QName()
    this.name.readExternal(in)
    val sizeAttr = in.readInt
    this.attributes = (1 to sizeAttr).map(x => {
      val name = new QName()
      name.readExternal(in)
      val value = in.readUTF
      (name, value)
    }).toMap[QName, String]
    val sizeNamespc = in.readInt
    this.namespaces = (1 to sizeNamespc).map( x => {
      val prefix = in.readUTF
      val namespaceURI = in.readUTF
      (prefix, namespaceURI)
    }).toMap[String, String]
  }

  def writeExternal(out: ObjectOutput) {
    this.name.writeExternal(out)
    out.writeInt(this.attributes.size)
    this.attributes.foreach[Unit]( x => {
      val (attributeName : QName, attributeValue : String) = x
      attributeName.writeExternal(out)
      out.writeUTF(attributeValue)
    })
    val namespc : Map[String, String]= if (this.namespaces == null) Map.empty else this.namespaces
    out.writeInt(namespc.size)
    namespc.foreach[Unit]( x => {
      val (prefix : String, namespaceURI : String) = x
      out.writeUTF(prefix)
      out.writeUTF(namespaceURI)
    })
    
  }



}

/** An element representing a closing tag */
@SerialVersionUID(32004)
case class EvElemEnd(var name : QName) extends XMLEvent {
  final def this() = this(null)
  final def appendWriter(sb: Writer) {
    sb.append('<')
    sb.append('/')
    if (name.prefix.length() != 0) {
      sb.append(name.prefix)
      sb.append(':')
    }
    sb.append(name.localPart)
    sb.append('>')
  }


  def readExternal(in: ObjectInput) {
    this.name = new QName()
    this.name.readExternal(in)
  }

  def writeExternal(out: ObjectOutput) {
    this.name.writeExternal(out)
  }
}
/** A text node is encountered */
@SerialVersionUID(32005)
case class EvText(var text : String) extends XMLEvent {
  final def this() = this(null)
  final def appendWriter(sb: Writer) {
    sb.append(text)
  }


  def readExternal(in: ObjectInput) {
    this.text = in.readUTF
  }

  def writeExternal(out: ObjectOutput) {
    out.writeUTF(this.text)
  }
}

/** An entity reference is encountered */
@SerialVersionUID(32006)
case class EvEntityRef(var entity: String) extends XMLEvent {
  final def this() = this(null)
  final def appendWriter(sb: Writer) {
    sb.append('&')
    sb.append(entity)
    sb.append(';')
  }

  def readExternal(in: ObjectInput) {
    this.entity = in.readUTF
  }

  def writeExternal(out: ObjectOutput) {
    out.writeUTF(this.entity)
  }
}

/** A processing instruction is encountered */
@SerialVersionUID(32007)
case class EvProcInstr(var target: String, var text: String) extends XMLEvent {
  final def this() = this(null, null)
  final def appendWriter(sb: Writer) {}

  final def readExternal(in: ObjectInput) {
    this.target = in.readUTF
    this.text = in.readUTF
  }

  final def writeExternal(out: ObjectOutput) {
    out.writeUTF(this.target)
    out.writeUTF(this.text)
  }
}

/** A comment is encountered */
@SerialVersionUID(32008)
case class EvComment(var text: String) extends XMLEvent {
  final def this() = this(null)
  final def appendWriter(sb: Writer) {
    sb.append("<!-- ")
    sb.append(text)
    sb.append(" -->")
  }
  final def readExternal(in: ObjectInput) {
    this.text = in.readUTF
  }

  final def writeExternal(out: ObjectOutput) {
    out.writeUTF(this.text)
  }
}

