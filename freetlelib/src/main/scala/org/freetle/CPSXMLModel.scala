 /*
  * Copyright 2010-2013 Lucas Bruand
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
import java.io._
import xml._
import java.lang.String
import javax.xml.stream.{XMLStreamWriter, XMLOutputFactory}
import com.ctc.wstx.stax.WstxOutputFactory

/**
 * This is a streaming Continuation Passing Transformation model.
 * It is capable of working over XML Events.
 *
 * CPSXMLModel specialize [[ org.freetle.CPSModel ]] for a [[ org.freetle.util.XMLEvent ]] alphabet.
 * it lets you define transformations by writing expressions using :
 *  - A small number of unit transforms
 *  - Operators that let you combine sub-expressions.
 *
 *  for example, you can extends the CPSXMLModel to define a specific Parser. :
 *
 *  {{{
 *  class TransformSampleParser extends CPSXMLModel[TransformSampleContext] with CPSMeta[TransformSampleContext] {
 *    def header : ChainedTransformRoot = <("catalog")
 *    def footer : ChainedTransformRoot = </("catalog")
 *    def element : ChainedTransformRoot = <("cd") ~ </("cd")
 *    def document :ChainedTransformRoot = header ~ ((element)+) ~ footer
 *    def transform : ChainedTransformRoot = (document).metaProcess(new SpaceSkipingMetaProcessor())
 *  }
 *  }}}
 *
 *  transform is a base rule. It uses a metaprocessor that decorates the transformation to
 *  skip all whitespace characters.
 *  document is a rule defined as recognizing :
 *   - a `header` rule (itself recognizing a catalog opening tag).
 *   - one or more `element` rules.
 *   - a `footer` rule (itself recognizing a catalog closing tag).
 *
 *  `element` recognizes an opening tag followed by a closing tag.
 *
 */
class CPSXMLModel[@specialized Context] extends CPSModelSerializable[XMLEvent, Context] {

  /**
   * Failure CFilter.
   * @throws ParsingFailure raised whenever parsing fails and this continuation is called.
   */
  class FailsCFilter extends CFilter {
    def apply(inputStream : CPSStream, context : Context) : CPSStream = {
      val inputStreamNoEmptyPositive = CPSStreamHelperMethods.removeWhileEmptyPositive(inputStream)
      val failure = if (inputStreamNoEmptyPositive.isEmpty) {
        new ParsingFailure("Parsing failed")
      } else {
        inputStreamNoEmptyPositive.head._1 match {
          case Some(x) => new ParsingFailure("Parsing failed offset [" +
                                             x.location.getCharacterOffset +
                                            "] (" + x.location.getColumnNumber +
                                            ":" + x.location.getLineNumber +
                                            ") token " + x.toString)
          case _ => new ParsingFailure("Parsing failed")
        }
      }
      throw failure
    }
  }

  /**
   * Take all the underlying nodes of the current event.
   * The deepfilter does return the matching end bracket.
   */
  class DeepFilter extends StatefulSelector[Int] {
    def metaProcess(metaProcessor: MetaProcessor) = metaProcessor.processTransform(this, () => { new DeepFilter() })
    def conditionToStop(depth: Int) = (depth <= 0)

    def accumulate(depth: Int, element: CPSElementOrPositive) : Int = depth + (element match {
      case Some(EvElemStart(_, _, _)) => +1
      case Some(EvElemEnd(_)) => -1
      case _ => 0
    })
    
    def initialState = 1
  }

  /**
   * Take all the underlying nodes of the current event.
   * The deepfilteruntil does not return the matching end bracket.
   */
  class DeepFilterUntil extends StatefulSelectorUntil[Int] {
    def metaProcess(metaProcessor: MetaProcessor) = metaProcessor.processTransform(this, () => { new DeepFilterUntil() })
    def conditionToStop(depth: Int) = (depth <= 0)

    def accumulate(depth: Int, element: CPSElementOrPositive) : Int = depth + (element match {
      case Some(EvElemStart(_, _, _)) => +1
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
    
    @inline def apply(inputStream : CPSStream, context : Context) : (CPSStream, Context) = {
      if (inputStream.isEmpty)
        (inputStream, context)
      else {
        val sr = CPSStreamHelperMethods.removeWhileEmptyPositive(inputStream)
        (sr.head._1.get) match {
          case EvText(txt) =>
            (Stream.cons( (sr.head._1, true), sr.tail), pushToContext(txt, context))
          case _ => (Stream.cons( (None, true), sr), pushToContext("", context))
        }
      }
    }

    def pushToContext(text : String, context : Context) : Context
  }

  /**
   * A base class to load attributes values to context.
   */
  abstract class TakeAttributesToContext(matcher : EvStartMatcher) extends ContextWritingTransform {
    def metaProcess(metaProcessor: MetaProcessor) = metaProcessor.processTransform(this, () => { this })

    @inline def apply(inputStream : CPSStream, context : Context) : (CPSStream, Context) = {
      if (inputStream.isEmpty)
        (inputStream, context)
      else {
        val inputStreamNoEmptyPositive = CPSStreamHelperMethods.removeWhileEmptyPositive(inputStream)
        val elem = inputStreamNoEmptyPositive.head._1.get
        if (matcher(elem)) {
          (elem) match {
            case EvElemStart(name, attributes, namespaces)  =>
              (Stream.cons( (inputStreamNoEmptyPositive.head._1, true), inputStreamNoEmptyPositive.tail), pushToContext(name, attributes, namespaces, context))
            case _ => (inputStream, context)
          }
        } else {
          (inputStream, context)
        }
      }
    }
    def pushToContext(name : QName, attributes : Map[QName, String], namespaces : Map[String, String], context : Context) : Context
  }

  /**
   * A companion object to PushNode class.
   * Internal.
   */
  private object PushNode {

    def serializeXML(nodeSeq : NodeSeq) : Stream[XMLEvent] = {
      ((nodeSeq map( serializeNodeXML(_))).toStream.flatten)
    }

    private def createAttributes(element : scala.xml.Elem) : Map[QName, String] = {
      if (element != null && element.attributes != null) {
        Map.empty ++ (element.attributes map (x => x match {
          case p:PrefixedAttribute => QName(namespaceURI = p.getNamespace(element), localPart = p.key, prefix = p.pre) -> p.value.mkString
          case u:UnprefixedAttribute => QName(localPart = u.key) -> u.value.mkString
        }))
      } else {
        null
      }
    }

    private def buildQName(element: Elem): QName = {
      if (element.prefix == null || element.prefix.isEmpty)
        new QName(element.scope.getURI(null), element.label)
      else
        new QName(element.scope.getURI(element.prefix), element.label, element.prefix)
    }

    def serializeNodeXML(node : Node) : Stream[XMLEvent] =
      node match {
      case element :  scala.xml.Elem //(prefix, label, attributes, scope, children)
            => {
                  val qName: QName = buildQName(element)
                  Stream.cons( new EvElemStart(qName,  createAttributes(element)),
                          Stream.concat(serializeXML(element.child),
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
      ((context :Context) => Some(context)) andThen nodeSeq andThen PushNode.serializeXML
  ) {
    override def metaProcess(metaProcessor: MetaProcessor) = metaProcessor.processTransform(this, () => { this })
  }

  /**
   * Push Formatted text from the context down to output stream.
   */
  class PushFormattedText(formatter: Context => String) extends PushFromContext(
    formatter andThen ((text:String) => Stream(new EvText(text)))
  ) {
    override def metaProcess(metaProcessor: MetaProcessor) = metaProcessor.processTransform(this, () => { this })
  }

  /**
   * Shortcut to output various objects downstream.
   */
  object > {
    def apply(formatter: Context => String) : PushFormattedText = {
      new PushFormattedText(formatter = formatter)
    }

    def apply(text : String) : PushText = {
      new PushText(text = text)
    }

    def apply(event : XMLEvent) : PushFromContext = new PushFromContext(context => Stream(event))

    def apply(events :Stream[XMLEvent]) : PushFromContext = new PushFromContext(context => events)
  }

  /**
   * Output text downstream.
   */
  class PushText(text: String) extends PushFromContext(x => Stream(new EvText(text))) {
    override def metaProcess(metaProcessor: MetaProcessor) = metaProcessor.processTransform(this, () => { this })
  }

  /**
   * A NameSpaceMatcher checks the namespace based on its qualified name.
   */
  type NameSpaceMatcher = QName => Boolean

  /**
   * This implicit NamespaceMatcher does no check at all on the namespace's qualified name.
   */
  implicit object DefaultNamespaceMatcher extends NameSpaceMatcher {
    def apply(name: QName) : Boolean = true
  }

  /**
   * An abstract matcher for all tags (starting and ending)
   */
  abstract class EvTagMatcher extends CPSElemMatcher


  /**
   * Matches an EvElemStart
   */
  abstract class EvStartMatcher(nameSpaceMatcher :NameSpaceMatcher) extends EvTagMatcher {
    /**
     * implement this to check whether the Element is the one we are looking for.
     */
    def testElem(name : QName, attributes : Map[QName, String], namespaces : Map[String, String]) : Boolean

    /**
     * Call the testElem method to check the event.
     */
    def apply(event: XMLEvent) : Boolean = {
      event match {
        case EvElemStart(name, attributes, namespaces) => {
           testElem(name, attributes, namespaces)
        }
        case _ => false
      }
    }
  }

  /**
   * A matcher that matches EvElemStarts based on their localPart.
   */
  class LocalPartEvStartMatcher(localPart : String)(implicit nameSpaceMatcher :NameSpaceMatcher) extends EvStartMatcher(nameSpaceMatcher) {
    def testElem(name: QName, attributes: Map[QName, String], namespaces : Map[String, String]) =
          localPart.equals(name.localPart) && nameSpaceMatcher(name)
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
   */
  implicit def string2EvStartMatcher(text : String)(implicit nameSpaceMatcher :NameSpaceMatcher) : EvStartMatcher = new LocalPartEvStartMatcher(text)(nameSpaceMatcher)
  /**
   * Matches an EvElemEnd
   */
  abstract class EvEndMatcher(nameSpaceMatcher :NameSpaceMatcher) extends EvTagMatcher {
    def testElem(name : QName) : Boolean

    def apply(event: XMLEvent) : Boolean = {
      event match {
        case EvElemEnd(name) => {
           testElem(name)
        }
        case _ => false
      }
    }
  }
  /**
   * A matcher that matches EvElemEnds based on their localPart.
   */
  class LocalPartEvEndMatcher(localPart : String)(implicit nameSpaceMatcher :NameSpaceMatcher) extends EvEndMatcher(nameSpaceMatcher) {
    def testElem(name: QName) = localPart.equals(name.localPart) && nameSpaceMatcher(name)
  }
  /**
   * Shortcut to take a closing tag based on the localpart.
   */
  object </ {

    def apply(matcher : EvEndMatcher) : ElementMatcherTaker = {
      new ElementMatcherTaker(matcher)
    }
    def apply(name : String)(implicit nameSpaceMatcher :NameSpaceMatcher) : ElementMatcherTaker = {
      apply(new LocalPartEvEndMatcher(name)(nameSpaceMatcher))
    }
  }
  /**
   * Shortcut to take text.
   * Can fake a empty text by using the ? cardinality operator.
   */
  val takeText = (new ElementMatcherTaker(new EvTextTypeMatcher()))?

  /**
   * Shortcut to take space or comment.
   */
  val takeSpace = new ElementMatcherTaker(new SpaceOrCommentMatcher())
  
  /**
   * Util class to build XMLResultStream, save etc...
   */
  object XMLResultStreamUtils {
    /**
     * converts an iterator of XMLEvent to a negative `CPSStream`.
     */
    def convertToCPSStream(input : Iterator[XMLEvent]) : CPSStream =
        (input map ((x :XMLEvent) => (Some(x), false))).toStream

    /**
     * Load a XMLResultStream from an InputStream
     */
    def loadXMLResultStream(inputStream : InputStream, xsdURL : String = null) : CPSStream =
        convertToCPSStream(new XMLEventStream(inputStream, xsdURL))

    /**
     * Load a XMLResultStream from a String.
     */
    def loadXMLResultStream(xmlString : String) : CPSStream =
        loadXMLResultStream(new ByteArrayInputStream(xmlString.getBytes))

    /**
     * Load a XMLResultStream from a Stream of char.
     */
    def loadXMLResultStream(charStream : =>Stream[Char]) : CPSStream =
        convertToCPSStream(new XMLEventStream(charStream))

    val CANNOTPARSE: String = "Could not parse the whole input."


    /**
     * Serialise a XMLResultStream into a XML form.
     */
    def serializeXMLResultStream(evStream : =>CPSStream, writer : Writer) {
      val outputFactory : WstxOutputFactory = new WstxOutputFactory()
      outputFactory.configureForSpeed()
      val xmlStreamWriter : XMLStreamWriter = outputFactory.createXMLStreamWriter(writer)
      serializeXMLResultStreamToXMLStreamWriter(evStream, xmlStreamWriter)
      xmlStreamWriter.close()
    }

    /**
     * SerializeXMLResultStreeam
     */
    def serializeXMLResultStreamToXMLStreamWriter(evStream : =>CPSStream, writer : XMLStreamWriter) {
      evStream foreach (_ match {
                case (Some(xmlEvent : XMLEvent), true) => xmlEvent.appendTo(writer)
                case (None, true) => (new EvComment("EmptyPositive")).appendTo(writer)
                case (Some(xmlEvent : XMLEvent), false) => if (xmlEvent.location != null)
                                        throw new ParsingFailure(CANNOTPARSE + " [" + xmlEvent.location.getColumnNumber +
                                                                                ":" + xmlEvent.location.getLineNumber +
                                                                                "/" + xmlEvent.location.getCharacterOffset +
                                                                                "]")
                                      else
                                        throw new ParsingFailure(CANNOTPARSE)
                case (None, false) => throw new ParsingFailure(CANNOTPARSE)
              })
    }

    /**
     * Deserialize from an objectInputStream serialized/binary XMLEvent. 
     */
    def rehydrate(objectInputStream : ObjectInputStream) : CPSStream = {
      val read = objectInputStream.readObject
      if (read != null) {
        Stream.cons( (Some((read).asInstanceOf[XMLEvent]), false), rehydrate(objectInputStream))
      } else {
        Stream.Empty
      }
    }

    /**
     * Serialize to an objectOutputStream serialized/binary XMLEvent.
     */
    def dehydrate(evStream: CPSStream, dataOut: ObjectOutputStream) {
      evStream.foreach(x => {dataOut.writeObject(x._1.get)})
    }
  }
  
}