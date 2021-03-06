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
import io.Source
import xml._
import javax.xml.stream.{XMLOutputFactory, XMLStreamWriter}


/**
 * This model can be extended to transform a Stream[Char] towards a Stream[XMLEvent]
 */
class CPSTranslateModel[Context] extends CPSModel[Either[Char, XMLEvent], Context] {

  /**
   * An implicit used to convert any char to a Element
   */
  implicit def convert(char : Char) : Either[Char, XMLEvent] = Left(char)

  /**
   * An implicit used to convert any XMLEvent to a Element
   */
  implicit def convert(xmlEvent : XMLEvent) : Either[Char, XMLEvent] = Right(xmlEvent)

  /**
   * Recognizes any char value.
   */
  val takeAnyChar = new ElementMatcherTaker((x :Either[Char, XMLEvent]) => x match {
            case Left(_) => true
            case _ => false
          })

  /**
   * Recognizes a Digit
   */
  val takeADigit = new ElementMatcherTaker((x :Either[Char, XMLEvent]) => x match {
            case Left(input) if '0' to '9' contains input => true
            case _ => false
          })


  /**
   * Recognizes a char.
   */
  class CharElementMatcher(val charInput : Char) extends CPSElemMatcher {
    def apply(x: Either[Char, XMLEvent] ) : Boolean = x match {
            case Left(`charInput`) => true
            case _ => false
          }
  }

  /**
   * Recognizes a constant Char.
   */
  private def constChar(constantChar:Char) : ChainedTransformRoot =
                    new ElementMatcherTaker(new CharElementMatcher(constantChar))


  /**
   * Recognizes a constant String.
   */
  def const(constantString :String) : ChainedTransformRoot = {
    constantString map (constChar(_)) reduce (
        (x : ChainedTransformRoot, y : ChainedTransformRoot) => new SequenceOperator(x,  y) )
  }


  /**
   * A basic element to recurse into the constTree.
   */
  private def makeBranch( c: Char, t : PrefixMap[ChainedTransformRoot]) : ChainedTransformRoot =
                                new SequenceOperator(constChar(c), constTree(t))

  /**
   * You can pass to a constTree a PrefixMap.
   * This prefixMap associates a constant String with a result.
   * The constTree factors out common prefix on the left for all entries in order for
   * the number of choices to be minimum.
   */
  def constTree(prefixMap :PrefixMap[ChainedTransformRoot]) : ChainedTransformRoot = {
    prefixMap.value match {
      case Some(x) => x
      case None => prefixMap.suffixes map ( x => makeBranch(x._1,x._2)  ) reduce(
                        (x : ChainedTransformRoot, y : ChainedTransformRoot) => new ChoiceOperator(x,  y) )
    }
  }

  /**
   * A base class to load text tokens to context.
   */
  abstract class TakeResultToContext extends ContextWritingTransform {
    def metaProcess(metaProcessor: MetaProcessor) = metaProcessor.processTransform(this, () => { this })

    @inline def apply(inputStream : CPSStream, context : Context) : (CPSStream, Context) = {
      if (inputStream.isEmpty)
        (inputStream, context)
      else {
        //val sr = CPSStreamHelperMethods.removeAllEmptyPositive(stream)
        val (shead, stail) = inputStream span (x => x._2)
        val sr = CPSStreamHelperMethods.removeAllEmptyPositive(shead)
        val result : String = (sr map ( (x :(Option[Either[Char, XMLEvent]], Boolean)) => x match {
            case (Some(Left(c)), true) => c
            case _ => ' '
          } )).mkString
        (CPSStreamHelperMethods.appendPositiveStream(stail), pushToContext(result, context))
      }
    }

    def pushToContext(text : String, context : Context) : Context
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

    def apply(event : XMLEvent) : PushFromContext = new PushFromContext(c => Stream(Right(event)))

    def apply(events :Stream[XMLEvent]) : PushFromContext = new PushFromContext(c => (events map (Right(_))))

    def apply(events :Context => Stream[XMLEvent]) : PushFromContext = new PushFromContext(c => (events(c) map (Right(_))))
  }

  /**
   * Push Formatted text from the context down to output stream.
   */
  class PushFormattedText(formatter: Context => String) extends PushFromContext(
    formatter andThen ((x:String) => Stream(Right(new EvText(x))))
  ) {
    override def metaProcess(metaProcessor: MetaProcessor) = metaProcessor.processTransform(this, () => { this })
  }
  /**
   * Output text downstream.
   */
  class PushText(text: String) extends PushFromContext(x => Stream(Right(new EvText(text)))) {
    override def metaProcess(metaProcessor: MetaProcessor) = metaProcessor.processTransform(this, () => { this })
  }

    /**
   * A companion object to PushNode class.
   * Internal.
   */
  private object PushNode {

    def serializeXML(nodeSeq : NodeSeq) : Stream[Either[Char,  XMLEvent]] = {
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

    private def buildQName(elem: Elem): QName = {
      if (elem.prefix == null || elem.prefix.isEmpty)
        new QName(elem.scope.getURI(null), elem.label)
      else
        new QName(elem.scope.getURI(elem.prefix), elem.label, elem.prefix)
    }

    def serializeNodeXML(node : Node) : Stream[Either[Char,  XMLEvent]] =
      node match {
      case elem :  scala.xml.Elem //(prefix, label, attributes, scope, children)
            => {
                  val qName: QName = buildQName(elem)
                  Stream.cons(Right(new EvElemStart(qName,  createAttributes(elem))),
                          Stream.concat(serializeXML(elem.child),
                            Stream(Right(new EvElemEnd(qName)))))
                }
      case text : scala.xml.Text => Stream(Right(new EvText(text.text)))
      case comment : scala.xml.Comment => Stream(Right(new EvComment(comment.text)))
      case pi : scala.xml.ProcInstr => Stream(Right(new EvProcInstr(pi.target, pi.proctext)))
      case entityRef : scala.xml.EntityRef => Stream(Right(new EvEntityRef(entityRef.entityName)))
      case atom : scala.xml.Atom[Any] => Stream(Right(new EvText(atom.text)))
      case _ => Stream(Right(new EvText("error" + node.getClass))) // TODO Throw exception.
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

  /**
   * Util class to build XMLResultStream, save etc...
   */
  object ResultStreamUtils {

    def convertCharToCPSStream(inputIter : Iterator[Char]) : CPSStream =
        (inputIter map ((x :Char) => (Some(Left(x)), false))).toStream
    /**
     * Load a XMLResultStream from an InputStream
     */
    def loadXMLResultStream(inputStream : InputStream) : CPSStream =
        convertCharToCPSStream(Source.fromInputStream(inputStream))

    /**
     * Load a XMLResultStream from a String.
     */
    def loadXMLResultStream(charStream : String) : CPSStream =
        loadXMLResultStream(new ByteArrayInputStream(charStream.getBytes))

    /**
     * Load a XMLResultStream from a String.
     */
    def loadXMLResultStream(charStream : =>Stream[Char]) : CPSStream =
        convertCharToCPSStream(charStream.iterator)

    /**
     * Serialise a XMLResultStream into a XML form.
     */
    def serializeXMLResultStream(evStream : =>CPSStream, writer : Writer) {
      val output : XMLOutputFactory = XMLOutputFactory.newInstance();
      val xmlStreamWriter : XMLStreamWriter = output.createXMLStreamWriter(writer)
      serializeXMLResultStreamToXMLStreamWriter(evStream, xmlStreamWriter)
      xmlStreamWriter.close()
    }

    /**
     * SerializeXMLResultStreeam
     */
    def serializeXMLResultStreamToXMLStreamWriter(evStream : =>CPSStream, writer : XMLStreamWriter) {
      evStream foreach (_ match {
        case (Some(Right(x : XMLEvent)), true) => x.appendTo(writer)
        case (_, false) => throw new ParsingFailure("Could not parse the whole input.")
        case _ => {}
      })
    }

    /**
     * Rehydrate from an objectInputStream serialized/binary XMLEvent.
     */
    def rehydrate(inputStream : ObjectInputStream) : CPSStream = {
      val read = inputStream.readObject
      if (read != null) {
        Stream.cons( (Some((read).asInstanceOf[XMLEvent]), false), rehydrate(inputStream))
      } else {
        Stream.Empty
      }
    }

    /**
     * Dehydriate to an objectOutputStream serialized/binary XMLEvent.
     */
    def dehydrate(evStream: CPSStream, dataOut: ObjectOutputStream) {
      evStream.foreach(x => {dataOut.writeObject(x._1.get)})
    }
  }

}