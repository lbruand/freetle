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
    def conditionToStop(depth: Int) = (depth <= 0)

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
        val sr = CPSStreamHelperMethods.removeWhileEmptyPositive(stream)
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
        val sr = CPSStreamHelperMethods.removeWhileEmptyPositive(stream)
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

  object PushNode {
        def serializeXML(nodeSeq : NodeSeq) : Stream[XMLEvent] = {
      ((nodeSeq map( serializeNodeXML(_))).toStream.flatten)
    }

    def serializeNodeXML(node : Node) : Stream[XMLEvent] =
      node match {
      case elem :  scala.xml.Elem //(prefix, label, attributes, scope, children)
            => {
                  val qName: QName = if (elem.prefix == null || elem.prefix.isEmpty)
                                        new QName(elem.scope.getURI(null), elem.label)
                                     else
                                        new QName(elem.scope.getURI(elem.prefix), elem.label, elem.prefix)
                  Stream.cons( new EvElemStart(qName,  null),
                          Stream.concat(serializeXML(elem.child),
                            Stream(new EvElemEnd(qName))))
                }
      case text : scala.xml.Text => Stream(new EvText(text.text))
      case comment : scala.xml.Comment => Stream(new EvComment(comment.text))
      case pi : scala.xml.ProcInstr => Stream(new EvProcInstr(pi.target, pi.proctext))
      case entityRef : scala.xml.EntityRef => Stream(new EvEntityRef(entityRef.entityName))
      case atom : scala.xml.Atom[Any] => Stream(new EvText(atom.text))
      case _ => Stream(new EvText("error" + node.getClass)) // TODO Throw exception.
    }
  }
  /**
   * Push a scala xml content down the pipeline.
   */
  @SerialVersionUID(599494944949L + 10 *19L)
  class PushNode(nodeSeq: Option[Context] => NodeSeq) extends PushFromContext(
      ((x :Context) => Some(x)) andThen nodeSeq andThen PushNode.serializeXML
  ) {
    override def metaProcess(metaProcessor: MetaProcessor) = metaProcessor.processTransform(this, () => { this })
  }

  class PushFormattedText(formatter: Context => String) extends PushFromContext(
    formatter andThen ((x:String) => Stream(new EvText(x)))
  ) {
    override def metaProcess(metaProcessor: MetaProcessor) = metaProcessor.processTransform(this, () => { this })
  }

  object > {
    def apply(formatter: Context => String) : PushFormattedText = {
      new PushFormattedText(formatter = formatter)
    }

    def apply(text : String) : PushText = {
      new PushText(text = text)
    }

    def apply(event : XMLEvent) : PushFromContext = new PushFromContext(c => Stream(event))

    def apply(events :Stream[XMLEvent]) : PushFromContext = new PushFromContext(c => events)
  }
  
  class PushText(text: String) extends PushFromContext(x => Stream(new EvText(text))) {
    override def metaProcess(metaProcessor: MetaProcessor) = metaProcessor.processTransform(this, () => { this })
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
      (new XMLEventStream(in) map (x => (Some(x), false))).toStream
    }

    /**
     * Load a XMLResultStream from a String.
     */
    def loadXMLResultStream(str : String) : XMLResultStream =
        loadXMLResultStream(new ByteArrayInputStream(str.getBytes))

    /**
     * Load a XMLResultStream from a String.
     */
    def loadXMLResultStream(str : =>Stream[Char]) : XMLResultStream =
        (new XMLEventStream(str) map (x => (Some(x), false))).toStream

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