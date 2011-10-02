 /*
  * Copyright 2010-2011 Lucas Bruand
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
package org.freetle

import util._
import xml.{Node, NodeSeq}
import java.io._

/**
 * This is a streaming Continuation Passing Transformation model.
 * It is capable of working over XML Events.
 */
class CPSXMLModel[@specialized Context] extends CPSModel[XMLEvent, Context] {
  /**
   * Used for backward compatibility.
   */
  type XMLResultStream = CPSStream

  /**
   * Take all the underlying nodes of the current event.
   * The deepfilter does return the matching end bracket.
   */
  class DeepFilter extends StatefulSelector[Int] {
    def metaProcess(metaProcessor: MetaProcessor) = metaProcessor.processTransform(this, () => { new DeepFilter() })
    def conditionToStop(depth: Int) = depth < 0

    def accumulate(depth: Int, element: CPSElementOrPositive) : Int = depth + (element match {
      case Some(EvElemStart(_, _)) => +1
      case Some(EvElemEnd(_)) => -1
      case _ => 0
    })
    
    def initialState = 1
  }

  /**
   * A base class to load text tokens to context.
   */
  abstract class TakeTextToContext extends ContextWritingTransform {
    def metaProcess(metaProcessor: MetaProcessor) = metaProcessor.processTransform(this, () => { this })
    
    @inline def apply(stream : CPSStream, context : Context) : (CPSStream, Context) = {
      if (stream.isEmpty)
        (stream, context)
      else {
        val sr = removeWhileEmptyPositive(stream)
        (sr.head._1.get) match {
          case EvText(txt) =>
            (Stream.cons( (sr.head._1, true), sr.tail), pushToContext(txt, context))
          case _ => (stream, context)
        }
      }
    }

    def pushToContext(text : String, context : Context) : Context
  }

  abstract class TakeAttributesToContext(matcher : EvStartMatcher) extends ContextWritingTransform {
    def metaProcess(metaProcessor: MetaProcessor) = metaProcessor.processTransform(this, () => { this })

    @inline def apply(stream : CPSStream, context : Context) : (CPSStream, Context) = {
      if (stream.isEmpty)
        (stream, context)
      else {
        val sr = removeWhileEmptyPositive(stream)
        val elem = sr.head._1.get
        if (matcher(elem)) {
          (elem) match {
            case EvElemStart(name, attributes)  =>
              (Stream.cons( (sr.head._1, true), sr.tail), pushToContext(name, attributes, context))
            case _ => (stream, context)
          }
        } else {
          (stream, context)
        }
      }
    }
    def pushToContext(name : QName, attributes : Map[QName, String], context : Context) : Context
  }

  /**
   * Push a scala xml content down the pipeline.
   */
  @serializable @SerialVersionUID(599494944949L + 10 *19L)
  class PushNode(nodeSeq: Option[Context] => NodeSeq) extends ContextReadingTransform {
    def metaProcess(metaProcessor: MetaProcessor) = metaProcessor.processTransform(this, () => { this })
    def serializeXML(nodeSeq : NodeSeq) : CPSStream = {
      (nodeSeq map( serializeNodeXML(_))).toStream.flatten
    }

    def serializeNodeXML(node : Node) : CPSStream =
      node match {
      case elem :  scala.xml.Elem //(prefix, label, attributes, scope, children)
            => {
                  val qName: QName = if (elem.prefix == null || elem.prefix.isEmpty)
                                        new QName(elem.scope.getURI(null), elem.label)
                                     else
                                        new QName(elem.scope.getURI(elem.prefix), elem.label, elem.prefix)
                  Stream.cons( (Some(new EvElemStart(qName,  null)), true),
                          Stream.concat(serializeXML(elem.child),
                            Stream((Some(new EvElemEnd(qName)), true))))
                }
      case text : scala.xml.Text => Stream((Some(new EvText(text.text)), true))
      case comment : scala.xml.Comment => Stream((Some(new EvComment(comment.text)), true))
      case pi : scala.xml.ProcInstr => Stream((Some(new EvProcInstr(pi.target, pi.proctext)), true))
      case entityRef : scala.xml.EntityRef => Stream((Some(new EvEntityRef(entityRef.entityName)), true))
      case atom : scala.xml.Atom[Any] => Stream((Some(new EvText(atom.text)), true))
      case _ => Stream((Some(new EvText("error" + node.getClass)), true)) // TODO Throw exception.
    }

    def partialapply(in : CPSStream, context : Context) : CPSStream = {
        serializeXML(nodeSeq.apply(Some(context))).append(in)
      }
  }

  class PushFormattedText(formatter: Context => String) extends ContextReadingTransform {
    def metaProcess(metaProcessor: MetaProcessor) = metaProcessor.processTransform(this, () => { this })

    def partialapply(in : CPSStream, context : Context) : CPSStream = {
        Stream.cons((Some(new EvText(formatter(context))), true), in)
      }
  }

  object > {
    def apply(formatter: Context => String) : PushFormattedText = {
      new PushFormattedText(formatter = formatter)
    }

    def apply(text : String) : PushText = {
      new PushText(text = text)
    }
  }
  
  class PushText(text: String) extends ContextReadingTransform {
    def metaProcess(metaProcessor: MetaProcessor) = metaProcessor.processTransform(this, () => { this })

    def partialapply(in : CPSStream, context : Context) : CPSStream = {
        Stream.cons((Some(new EvText(text)), true), in)
      }
  }
  abstract class EvStartMatcher extends CPSElemMatcher {
    def testElem(name : QName, attributes : Map[QName, String]) : Boolean

    def apply(event: XMLEvent) : Boolean = {
      event match {
        case EvElemStart(name, attributes) => {
           testElem(name, attributes)
        }
        case _ => false
      }
    }
  }

  class LocalPartEvStartMatcher(localPart : String) extends EvStartMatcher {
    def testElem(name: QName, attributes: Map[QName, String]) = localPart.equals(name.localPart) 
  }
  /**
   * Shortcut to take an opening tag based on the localpart.
   */
  object < {
    
    def apply(evStartMatcher : EvStartMatcher) : ElementMatcherTaker = {
      new ElementMatcherTaker(
          evStartMatcher
        )
    }

  }

  /**
   * This allows for automatic conversion toward an EvStartMatcher from a String.
   * TODO : Could add other matching on namespace, prefix, etc...
   */
  implicit def string2EvStartMatcher(s : String) : EvStartMatcher = new LocalPartEvStartMatcher(s)
  /**
   * Shortcut to take a closing tag based on the localpart.
   */
  object </ {
    def evEndMatcher(name : String)(event : XMLEvent) = {
      event match {
        case EvElemEnd(testName) if (name.equals(testName.localPart)) => true
        case _ => false
      }
    }
    def apply(name : String) = {
      new ElementMatcherTaker(evEndMatcher(name))
    }
  }
  /**
   * Shortcut to take text.
   */
  val takeText = new ElementMatcherTaker(new EvTextTypeMatcher())

  /**
   * Shortcut to take space or comment.
   */
  val takeSpace = new ElementMatcherTaker(new SpaceOrCommentMatcher())
  
  /**
   * Util class to build XMLResultStream, save etc...
   */
  object XMLResultStreamUtils {
    /**
     * Load a XMLResultStream from an InputStream
     */
    def loadXMLResultStream(in : InputStream) : XMLResultStream = {
      Stream.fromIterator(new XMLEventStream(in) map (x => (Some(x), false)))
    }

    /**
     * Load a XMLResultStream from a String.
     */
    def loadXMLResultStream(str : String) : XMLResultStream =
        loadXMLResultStream(new ByteArrayInputStream(str.getBytes))

    /**
     * Serialise a XMLResultStream into a XML form.
     */
    def serializeXMLResultStream(evStream : =>XMLResultStream, writer : Writer) : Unit = {
      evStream foreach (_._1 match {
                case Some(x : XMLEvent) => x.appendWriter(writer)
                case _ => (new EvComment("EmptyPositive")).appendWriter(writer)
              })
    }

    /**
     * Deserialize from an objectInputStream serialized/binary XMLEvent. 
     */
    def rehydrate(in : ObjectInputStream) : XMLResultStream = {
      val read = in.readObject
      if (read != null) {
        Stream.cons( (Some((read).asInstanceOf[XMLEvent]), false), rehydrate(in))
      } else {
        Stream.Empty
      }
    }

    /**
     * Serialize to an objectOutputStream serialized/binary XMLEvent.
     */
    def dehydrate(evStream: XMLResultStream, dataOut: ObjectOutputStream): Unit = {
      evStream.foreach(x => {dataOut.writeObject(x._1.get)})
    }
  }
  
}