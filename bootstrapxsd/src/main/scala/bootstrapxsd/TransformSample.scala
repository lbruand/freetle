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
package bootstrapxsd

import org.freetle.CPSXMLModel
import org.freetle.meta.CPSMeta
import java.io._
import org.freetle.util.{QName, EvElemStart, EvText}
import javax.xml.XMLConstants

case class TransformSampleContext(name : String = "", elemType : String = null)


/**
 * A sample transformation for freetle
 */
class TransformSampleParser extends CPSXMLModel[TransformSampleContext] with CPSMeta[TransformSampleContext] {
  val NS = "http://www.w3.org/2001/XMLSchema"
  val p = XMLConstants.DEFAULT_NS_PREFIX
   /**
   * A base class to load text tokens to context.
   */
  class TakeNameAttributeToContext(attributeName : String, matcher : EvStartMatcher) extends ContextWritingTransform {
    def metaProcess(metaProcessor: MetaProcessor) = metaProcessor.processTransform(this, () => { this })

    @inline def apply(stream : CPSStream, context : TransformSampleContext) : (CPSStream, TransformSampleContext) = {
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

    def pushToContext(name : QName, attributes : Map[QName, String], context : TransformSampleContext) : TransformSampleContext = {

      var attriName: Option[String] = attributes.get(QName("", attributeName, ""))
      attriName match {
        case Some(atname) => context.copy(name = atname)
        case None => context
      }

    }
  }
  
  def header : ChainedTransformRoot = <("schema")
  def footer : ChainedTransformRoot = </("schema")
  def innerElement : ChainedTransformRoot = <("complexType") ~ <("sequence") ~ ((element)*) ~ </("sequence") ~ </("complexType")
  val elementWithoutAttributeTypeMatcher = new EvStartMatcher() {
    def testElem(name : QName, attributes : Map[QName, String]) : Boolean = "element".equals(name.localPart) &&
        !attributes.contains(QName("", "type", ""))
  }
  def elementWithoutAttributeType : ChainedTransformRoot = <(elementWithoutAttributeTypeMatcher)
  def elementWithAttributeType : ChainedTransformRoot = <(new EvStartMatcher() {
    def testElem(name : QName, attributes : Map[QName, String]) : Boolean = "element".equals(name.localPart) &&
        attributes.contains(QName("", "type", ""))
  })
  def endElement : ChainedTransformRoot = </("element") 
  def element : ChainedTransformRoot = (elementWithAttributeType | (elementWithoutAttributeType ~ innerElement)) ~ endElement
  /*new TakeNameAttributeToContext("element", "name", "type")*/
  def document :ChainedTransformRoot = header ~ ((element)*) ~ footer 
  def transform : ChainedTransformRoot = (document).metaProcess(new SpaceSkipingMetaProcessor())
}

class TransformSampleTransformer extends TransformSampleParser {
  //override def header : ChainedTransformRoot =
  override def endElement : ChainedTransformRoot = (super.endElement) -> new DropFilter()
  override def elementWithoutAttributeType : ChainedTransformRoot = (new TakeNameAttributeToContext("name", elementWithoutAttributeTypeMatcher)) -> (new DropFilter() ~ new PushFormattedText( context =>
                                                    "def element%s : ChainedTransformRoot = <(\"%s\") ~ inner%s ~ </(\"%s\")\n" format (
                                                            context.name.capitalize,
                                                            context.name,
                                                            context.name.capitalize,
                                                            context.name)))
}

object TransformSampleMain extends TransformSampleTransformer {
  def usage() = {
    println("transform - Freetle sample transformation")
    println("usage : transform <input file> [output file]")
  }
  
  def main(args: Array[String]) {
    if (args.length > 2 || args.length == 0) {
      println("Wrong number of arguments")
      usage()
    } else {
      val input = new FileInputStream(args(0))
      try {
        val output = if (args.length == 2) new FileOutputStream(args(1)) else System.out
        val context = new TransformSampleContext()
        val transform = new TransformSampleTransformer()
        def inStream = XMLResultStreamUtils.loadXMLResultStream(input)
        def outStream = transform.transform(new CFilterIdentity(), new CFilterIdentity())(inStream, context)
        val sw = new OutputStreamWriter(output)
        XMLResultStreamUtils.serializeXMLResultStream(outStream, sw)
        sw.flush
      } finally {
        input.close
      }
    }
  }
}
