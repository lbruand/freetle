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

case class TransformSampleContext(name : String = "")


/**
 * A sample transformation for freetle
 */
class TransformSampleParser extends CPSXMLModel[TransformSampleContext] with CPSMeta[TransformSampleContext] {
  val NS = "http://www.w3.org/2001/XMLSchema"
  val p = XMLConstants.DEFAULT_NS_PREFIX
   /**
   * A base class to load text tokens to context.
   */
  class TakeTextToContext extends ContextWritingTransform {
    def metaProcess(metaProcessor: MetaProcessor) = metaProcessor.processTransform(this, () => { this })

    @inline def apply(stream : CPSStream, context : TransformSampleContext) : (CPSStream, TransformSampleContext) = {
      if (stream.isEmpty)
        (stream, context)
      else {
        val sr = removeWhileEmptyPositive(stream)
        (sr.head._1.get) match {
          case EvElemStart(name, attributes) if ("element".equals(name.localPart)) =>
            (Stream.cons( (sr.head._1, true), sr.tail), pushToContext(attributes.get(QName(NS, "name" ,p)).get, context))
          case _ => (stream, context)
        }
      }
    }

    def pushToContext(text : String, context : TransformSampleContext) : TransformSampleContext =
        context.copy(name = text)
  }
  
  def header : ChainedTransformRoot = <("schema")
  def footer : ChainedTransformRoot = </("schema")
  def element : ChainedTransformRoot = <("element") ~ </("element")
  def document :ChainedTransformRoot = header ~ ((element)*) ~ footer 
  def transform : ChainedTransformRoot = (document).metaProcess(new SpaceSkipingMetaProcessor())
}

class TransformSampleTransformer extends TransformSampleParser {
  override def element : ChainedTransformRoot = (super.element) -> new DropFilter()
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
        val transform = new TransformSampleParser()
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
