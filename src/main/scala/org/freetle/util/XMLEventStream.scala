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
import scala.xml.{MetaData, NamespaceBinding}
import java.io.{InputStream, Reader}
import javax.xml.namespace.QName
import collection.immutable.HashMap

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

 	object StreamSource  {
	    def fromIterator(s: Iterator[Char]): Source = {
    		lazy val it = s
    		new Source {
    			override def reset() = fromIterator(s)
    			val iter = it
    		}
	    }
	}


/**
 * 
 * NB: You can create a stream from this using Stream.fromIterator()
 * TODO Should add other constructors.
 */
class XMLEventStream(src: Any) extends Iterator[XMLEvent] {

  lazy val factory = XMLInputFactory.newInstance()
  lazy val input : XMLStreamReader = src match {
      case in : InputStream => factory.createXMLStreamReader(in)
      case src :Source => factory.createXMLStreamReader("hello.xml", new SourceReader(src))
  }
  type Attributes = Map[QName, String]
  def buildAttributes(input : XMLStreamReader) : Attributes = {
    List.range(0, input.getAttributeCount).map(
      x => (input.getAttributeName(x), input.getAttributeValue(x))
      ).toMap    
  }
  //input.getNamespaceContext
  def buildEvent(input:XMLStreamReader) : XMLEvent = {
    val eventType = input.getEventType
	  val event : XMLEvent = eventType match {
	    case XMLStreamConstants.START_ELEMENT => new EvElemStart(input.getName,
                                                               buildAttributes(input)
                                                               )
      case XMLStreamConstants.END_ELEMENT => new EvElemEnd(input.getName)
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
