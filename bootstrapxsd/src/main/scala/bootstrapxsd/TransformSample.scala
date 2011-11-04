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

case class TransformSampleContext(
        name : String = "",
        elemType : String = null,
        minOccurs : String = null,
        maxOccurs : String = null)


/**
 * Xsd bootstrap parser.
 */
class TransformSampleParser extends CPSXMLModel[TransformSampleContext] with CPSMeta[TransformSampleContext] {
  val NS = "http://www.w3.org/2001/XMLSchema"
  val p = XMLConstants.DEFAULT_NS_PREFIX

   /**
   * A base class to load attributes to context.
   */
  class TakeSchemaAttributesToContext(matcher : EvStartMatcher) extends TakeAttributesToContext(matcher) {

    def pushToContext(name : QName, attributes : Map[QName, String], context : TransformSampleContext) : TransformSampleContext = {
      var nContext = context 
      val attriName: Option[String] = attributes.get(QName("", "name", ""))
      nContext = attriName match {
        case Some(atname) => nContext.copy(name = atname)
        case None => nContext
      }
      val attriType: Option[String] = attributes.get(QName("", "type", ""))
      nContext = attriType match {
        case Some(attype) => nContext.copy(elemType = attype)
        case None => nContext
      }
      val minOccurs: Option[String] = attributes.get(QName("", "minOccurs", ""))
      nContext = attriType match {
        case Some(atMinOccurs) => nContext.copy(minOccurs = atMinOccurs)
        case None => nContext
      }
      val maxOccurs: Option[String] = attributes.get(QName("", "maxOccurs", ""))
      nContext = attriType match {
        case Some(atMaxOccurs) => nContext.copy(maxOccurs = atMaxOccurs)
        case None => nContext
      }
      nContext
    }
  }

  
  def startSchema : ChainedTransformRoot = <("schema")
  def endSchema : ChainedTransformRoot = </("schema")


  val elementWithAttributeTypeMatcher = new EvStartMatcher() {
    def testElem(name : QName, attributes : Map[QName, String]) : Boolean = "element".equals(name.localPart) &&
        attributes.contains(QName("", "type", ""))
  }

  val complexTypeWithAttributeNameMatcher = new EvStartMatcher() {
    def testElem(name : QName, attributes : Map[QName, String]) : Boolean = "complexType".equals(name.localPart) &&
        attributes.contains(QName("", "name", ""))
  }


  def elementWithAttributeType : ChainedTransformRoot = <(elementWithAttributeTypeMatcher)
  def endElement : ChainedTransformRoot = </("element") 

  def element : ChainedTransformRoot = elementWithAttributeType ~ endElement

  def startSequence : ChainedTransformRoot = <("sequence")
  def endSequence : ChainedTransformRoot = </("sequence")
  def startChoice : ChainedTransformRoot = <("choice")
  def endChoice : ChainedTransformRoot = </("choice")
  def startComplexType : ChainedTransformRoot = <("complexType")
  def endComplexType : ChainedTransformRoot = </("complexType")
  def sequence : ChainedTransformRoot = startSequence ~ subitems ~ endSequence
  def choice : ChainedTransformRoot = startChoice ~ subitems ~ endChoice
  def complexType : ChainedTransformRoot = startComplexType ~ (sequence | choice) ~ endComplexType
  def subitems : ChainedTransformRoot =  (element | complexType)*
  def document :ChainedTransformRoot = startSchema ~ (subitems) ~ endSchema
  def transform : ChainedTransformRoot = (document).metaProcess(new SpaceSkipingMetaProcessor())
}

/**
 * Overloads the parser with handler to transform the streams.
 */
class TransformSampleTransformer(val packageName : String, val schemaName : String) extends TransformSampleParser {

  override def startComplexType : ChainedTransformRoot = (new TakeSchemaAttributesToContext(complexTypeWithAttributeNameMatcher)) -> (new DropFilter() ~ new PushFormattedText( context =>
                                                                "class %s " format (context.name.capitalize))
                                                            )

  override def startChoice : ChainedTransformRoot = (super.startChoice) -> (drop ~ >("extends ChoiceBaseType {\n"))
  override def endChoice : ChainedTransformRoot = (super.endChoice) -> drop
  override def startSequence : ChainedTransformRoot = (super.startSequence) -> (drop ~ >("extends SequenceBaseType {\n"))

  override def endSequence : ChainedTransformRoot = (super.endSequence) -> drop

  override def endComplexType : ChainedTransformRoot = (super.endComplexType) -> (drop ~ >( context => "}\n" ))

  override def endElement : ChainedTransformRoot = (super.endElement) -> drop
  
  override def elementWithAttributeType : ChainedTransformRoot = (new TakeSchemaAttributesToContext(elementWithAttributeTypeMatcher)) ->
                                                (drop ~ >( context =>

                                                    "def element%s : ChainedTransformRoot = <(\"%s\") ~ %s ~ </(\"%s\")\nlist += element%s\n" format (
                                                            context.name.capitalize,
                                                            context.name,
                                                            if (context.elemType.endsWith(":string") ) "takeText" else {"(new %s())()" format (context.elemType.capitalize) } ,
                                                            context.name,
                                                            context.name.capitalize)

                                                    ))
  override def startSchema : ChainedTransformRoot = (super.startSchema) -> (drop ~ >( context => "package %s\nclass %sSchema[Context] extends AbstractXMLSchema[Context] {\n" format (packageName, schemaName.capitalize)))
  override def endSchema : ChainedTransformRoot = (super.endSchema) -> (drop ~ >( context => "}\n" ))
}

object TransformSampleMain extends TransformSampleTransformer(packageName = "bootstrapxsd", schemaName = "Note") {
  def usage() = {
    println("transform - XSD Bootstrap transformation")
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
        val transform = this
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
