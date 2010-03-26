package org.freetle.util
import scala.io.Source
import javax.xml.stream.XMLInputFactory
import javax.xml.stream.XMLStreamReader
import javax.xml.stream.XMLStreamConstants
import java.io.Reader
import scala.xml.{MetaData, NamespaceBinding}

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
    			def reset() = fromIterator(s)
    			val iter = it
    		}
	    }
	}


/**
 * 
 * NB: You can create a stream from this using Stream.fromIterator()
 */
class XMLEventStream(src: Source) extends Iterator[XMLEvent] {
  lazy val factory = XMLInputFactory.newInstance();
  lazy val input : XMLStreamReader = factory.createXMLStreamReader("hello.xml", new SourceReader(src));
  def buildEvent(input:XMLStreamReader) : XMLEvent = {
    val eventType = input.getEventType
	  
	  eventType match {
	    case XMLStreamConstants.START_ELEMENT => new EvElemStart(input.getPrefix, input.getLocalName, null, null) 
      case XMLStreamConstants.END_ELEMENT => new EvElemEnd(input.getPrefix, input.getLocalName)
      case XMLStreamConstants.CHARACTERS => new EvText(input.getText)
      case XMLStreamConstants.COMMENT => new EvComment(input.getText)
      case XMLStreamConstants.PROCESSING_INSTRUCTION => new EvProcInstr(input.getPITarget, input.getPIData)
      case XMLStreamConstants.START_DOCUMENT => null      
        // TODO Add start and end document.
	    case _ => null
	  } 
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
