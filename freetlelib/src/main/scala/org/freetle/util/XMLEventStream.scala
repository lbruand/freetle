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
import scala.io.Source
import org.codehaus.stax2.{XMLStreamReader2, XMLInputFactory2}
import com.ctc.wstx.stax.WstxInputFactory
import javax.xml.stream.{XMLStreamReader, XMLStreamConstants}
import java.lang.String
import java.io.{File, InputStream, Reader}
import java.net.URL
import org.codehaus.stax2.validation.{XMLValidationSchema, XMLValidationSchemaFactory}

//import javax.xml.stream.XMLInputFactory
//import javax.xml.stream.XMLStreamReader

/**
 * Helper class to read a Source as a Reader.
 */
class SourceReader(src: Source) extends Reader {
  override def read(arr : Array[Char], start : Int, sz : Int) : Int = {
    var i = start
    while (src.hasNext && i - start < sz) {
      arr(i) = src.next()
      i += 1
    }
    if (i - start == 0 && !src.hasNext)
      -1
    else
      i - start
  }
  
  override def close() {
  }
  
}

/**
 * Helper class to read a Source as a Reader.
 */
class StreamReader(stream: =>Stream[Char]) extends Reader {
  var src = stream
  override def read(arr : Array[Char], start : Int, sz : Int) : Int = {
    var i = start
    while (!src.isEmpty && i - start < sz) {
      arr(i) = src.head
      src = src.tail
      i += 1
    }
    if (i - start == 0 && src.isEmpty)
      -1
    else
      i - start
  }

  override def close() {
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

object XMLEventStream {
  val factory =  new WstxInputFactory()
  factory.configureForSpeed()
  factory.getConfig.doCoalesceText(true)


}
/**
 * Transform a Source a XMLEvent Iterator for the purpose of making it a Stream afterward.
 * NB: You can create a stream from this using Stream.fromIterator().
 */
final class XMLEventStream(src: Any, xsdURL : String = null) extends Iterator[XMLEvent] {


  val input : XMLStreamReader2 = src match {
      case in : InputStream => XMLEventStream.factory.createXMLStreamReader(in).asInstanceOf[XMLStreamReader2]
      case stream : Stream[Char] => XMLEventStream.factory.createXMLStreamReader(new StreamReader(stream)).asInstanceOf[XMLStreamReader2]
      case src :Source => XMLEventStream.factory.createXMLStreamReader("default.xml", new SourceReader(src)).asInstanceOf[XMLStreamReader2]
  }
  if (xsdURL != null) {
    val validationFactory = XMLValidationSchemaFactory.newInstance(XMLValidationSchema.SCHEMA_ID_W3C_SCHEMA)
    val xmlSchema: XMLValidationSchema = validationFactory.createSchema(new URL(xsdURL))
    input.validateAgainst(xmlSchema)
  }

  type Attributes = Map[QName, String]
  type Namespaces = Map[String,  String]

  @inline private def fromJavaQName(qn : javax.xml.namespace.QName) : org.freetle.util.QName = {
    new QName(namespaceURI = qn.getNamespaceURI,
      localPart = qn.getLocalPart,
      prefix = qn.getPrefix)
  }
  
  @inline private def buildAttributes(input : XMLStreamReader) : Attributes = {
    List.range[Int](0, input.getAttributeCount).map(
      x => (fromJavaQName(input.getAttributeName(x)), input.getAttributeValue(x))
      ).toMap
  }
  
  @inline private def buildNamespaces(input : XMLStreamReader) : Namespaces = {
    List.range[Int](0, input.getNamespaceCount).map(
      x => (input.getNamespacePrefix(x), input.getNamespaceURI(x))
    ).toMap
  }

  @inline private def buildEvent(input:XMLStreamReader) : XMLEvent = {
    val eventType = input.getEventType
	  val event : XMLEvent = eventType match {
	    case XMLStreamConstants.START_ELEMENT => new EvElemStart(fromJavaQName(input.getName),
                                                               buildAttributes(input),
                                                               buildNamespaces(input)
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
  
  override def hasNext: Boolean = input.hasNext
}
