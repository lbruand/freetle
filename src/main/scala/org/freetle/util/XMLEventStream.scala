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
import scala.io.Source
import javax.xml.stream.XMLInputFactory
import javax.xml.stream.XMLStreamReader
import javax.xml.stream.XMLStreamConstants
import java.io.{InputStream, Reader}

/**
 * Helper class to read a Source as a Reader.
 */
class SourceReader(src: Source) extends Reader {
  override def read(arr : Array[Char], start : Int, sz : Int) : Int = {
    var i = start
    while (src.hasNext && i - start < sz) {
      arr(i) = src.next
      i += 1
    }
    if (i - start == 0 && !src.hasNext)
      -1
    else
      i - start
  }
  
  override def close() :Unit = {
  }
  
}

/**
 * Create a Source from a Char Iterator.
 */
object StreamSource {
  def fromIterator(s: Iterator[Char]): Source = {
    lazy val it = s
    new Source {
      override def reset() = fromIterator(s)
      val iter = it
    }
  }
}


/**
 * Transform a Source a XMLEvent Iterator for the purpose of making it a Stream afterward.
 * NB: You can create a stream from this using Stream.fromIterator().
 */
class XMLEventStream(src: Any) extends Iterator[XMLEvent] {

  lazy val factory = XMLInputFactory.newInstance()
  lazy val input : XMLStreamReader = src match {
      case in : InputStream => factory.createXMLStreamReader(in)
      case src :Source => factory.createXMLStreamReader("default.xml", new SourceReader(src))
  }
  type Attributes = Map[QName, String]

  @inline final private def fromJavaQName(qn : javax.xml.namespace.QName) : org.freetle.util.QName = {
    new QName(namespaceURI = qn.getNamespaceURI,
      localPart = qn.getLocalPart,
      prefix = qn.getPrefix)
  }
  
  @inline final private def buildAttributes(input : XMLStreamReader) : Attributes = {
    List.range(0, input.getAttributeCount).map(
      x => (fromJavaQName(input.getAttributeName(x)), input.getAttributeValue(x))
      ).toMap    
  }

  @inline final private def buildEvent(input:XMLStreamReader) : XMLEvent = {
    val eventType = input.getEventType
	  val event : XMLEvent = eventType match {
	    case XMLStreamConstants.START_ELEMENT => new EvElemStart(fromJavaQName(input.getName),
                                                               buildAttributes(input)
                                                               )
      case XMLStreamConstants.END_ELEMENT => new EvElemEnd(fromJavaQName(input.getName))
      case XMLStreamConstants.CHARACTERS => new EvText(input.getText)
      case XMLStreamConstants.COMMENT => new EvComment(input.getText)
      case XMLStreamConstants.PROCESSING_INSTRUCTION => new EvProcInstr(input.getPITarget, input.getPIData)
      case XMLStreamConstants.START_DOCUMENT => null      
        // TODO Add start and end document.
	    case _ => null
	  }
    if (event != null) {
      event.location = input.getLocation
      event.namespaceContext = input.getNamespaceContext
    }
    event
  }
  def next(): XMLEvent = {	  
	  var event = buildEvent(input)
    if (event == null) {
      input.next
      event = buildEvent(input)
    }
	  input.next
	  event
  }
  
  override def hasNext() : Boolean = input.hasNext
}
