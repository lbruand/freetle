package org.freetle.sample

import org.freetle.CPSXMLModel
import org.freetle.meta.CPSMeta
import java.io._

case class TransformSampleContext
/**
 * A sample transformation for freetle
 */
class TransformSampleParser extends CPSXMLModel[TransformSampleContext] with CPSMeta[TransformSampleContext] {
  def header : ChainedTransformRoot = <("catalog")
  def footer : ChainedTransformRoot = </("catalog")
  def element : ChainedTransformRoot = <("cd") ~ </("cd")
  def document :ChainedTransformRoot = header ~ ((element)+) ~ footer 
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
