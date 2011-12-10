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

import meta.CPSMeta
import org.junit._
import Assert._
import util._
import javax.xml.transform.TransformerFactory
import javax.xml.transform.stream.{StreamResult, StreamSource}
import java.io.{StringWriter, StringReader}
import testing.Benchmark
import compat.Platform
import net.sf.joost.trax.TransformerFactoryImpl


@SerialVersionUID(599494944949L + 30000L)
case class FreetleCaseBenchmarkContext(title : String = null, artist : String = null)

abstract class BaseCaseBenchmarkTransform
          extends CPSXMLModel[FreetleCaseBenchmarkContext]
              with CPSMeta[FreetleCaseBenchmarkContext] {

    //val transform = new CFilterIdentity()
    val transform : CFilter
  
    def run(catalogSource : String) : String = {

      val context = new FreetleCaseBenchmarkContext()
      def inStream = XMLResultStreamUtils.loadXMLResultStream(catalogSource)
      def outStream = transform(inStream, context)
      val sb = new StringWriter()
      XMLResultStreamUtils.serializeXMLResultStream(outStream, sb)      
      var r = sb.toString
      r
    }

}

class IdentityCaseBenchmarkTransform extends BaseCaseBenchmarkTransform {
  override val transform = new CFilterIdentity()
}

class FreetleCaseBenchmarkTransform extends BaseCaseBenchmarkTransform {
    /**
     * Real transform construction.
     */
    override val transform = {
      val titleTaker = new TakeTextToContext() {
        def pushToContext(text : String, context : FreetleCaseBenchmarkContext) : FreetleCaseBenchmarkContext = {
            context.copy(title = text)
        }
      }

      val artistTaker = new TakeTextToContext() {
        def pushToContext(text : String, context : FreetleCaseBenchmarkContext) : FreetleCaseBenchmarkContext = {
            context.copy(artist = text)
        }
      }

      val t = (<("catalog") -> drop
              )~
                  (((( (<("cd") ~
                      <("title") ~
                          titleTaker ~
                      </("title") ~
                      <("artist") ~
                          artistTaker ~
                      </("artist") ~
                      <("country") ~
                           takeText ~
                      </("country") ~
                      <("company") ~
                           takeText ~
                      </("company") ~
                      <("price") ~
                           takeText ~
                      </("price") ~
                      <("year") ~
                           takeText ~
                      </("year") ~
                    </("cd"))  -> drop) ~ new PushNode(
                       (x : Option[FreetleCaseBenchmarkContext]) => {
                         x match {

                                                    case Some(context) =>
<cd>
 <title>{context.title}</title>
 <artist>{context.artist}</artist>
</cd>
                                                    case _ => <cd></cd>
                         }
                       }
                    )
                    )
                  )  *) ~
              (</("catalog") -> drop
                      )
      val filterIdentity = new CFilterIdentity()
      val m = t.metaProcess(new SpaceSkipingMetaProcessor())
      m(filterIdentity, filterIdentity)
    }
}
/**
 * A test to benchmark the performance of the system.
 */
@Test
class BenchmarkTest {
  val catalogHeader = """<?xml version="1.0" encoding="UTF-8"?>
<catalog>"""

  val catalogFooter = """</catalog>
"""

  val random = new RandomUtils()
  def buildRandomCD(sb : StringBuilder) : Unit = {
    sb.append("<cd>\n")

    sb.append("<title>the ")
    random.nextStringLetterOrDigit(sb, 22)
    sb.append(" album</title>\n")

    sb.append("<artist>")
    random.nextStringLetterOrDigit(sb, 15)
    sb.append(" and the band</artist>\n")

    sb.append("<country>")
    random.nextStringLetterOrDigit(sb, 2)
    sb.append("</country>\n")

    sb.append("<company>")
    random.nextStringLetterOrDigit(sb, 15)
    sb.append(" inc.</company>\n")

    sb.append("<price>")
    sb.append("" + (random.nextInt(1000) + 1000))
    sb.append("</price>\n")

    sb.append("<year>")
    sb.append("" + (1950 + random.nextInt(60)))
    sb.append("</year>\n")

    sb.append("</cd>\n")
  }
  
  def buildRandomCatalog(n : Int) : String = {
    var sb = new StringBuilder(catalogHeader.length+ catalogFooter.length +
                              n*205)
    sb.append(catalogHeader)
    (0 to (n-1)).foreach(x => buildRandomCD(sb))
    sb.append(catalogFooter)
    sb.result()
  }
  
  abstract class FreetleBenchmark(val nCatalog : Int = 3) extends Benchmark {
    val name : String
    var catalogSource : String = null
  }

  class STXCaseBenchmark(override val nCatalog : Int = 3) extends FreetleBenchmark(nCatalog : Int) {
    override val name = "stx"
    override def prefix = "STX = "
    val stxSource = """<?xml version="1.0" encoding="UTF-8"?>
    <stx:transform xmlns:stx="http://stx.sourceforge.net/2002/ns"
               xmlns:joost="http://joost.sf.net/extension"
               xmlns:js="urn:javascript"
               version="1.0">

  <stx:template match="/">
    <stx:element name="html">
      <stx:element name="body">
        <stx:process-children />        
      </stx:element>
    </stx:element>
  </stx:template>

  <stx:variable name="title" />
  <stx:variable name="artist"/>

  <stx:template match="title">
    <stx:assign name="title" select="." />
  </stx:template>
  
  <stx:template match="artist">
    <stx:assign name="artist" select="." />
  </stx:template>

  <stx:template match="cd">
          <stx:process-children />
          <stx:element name="tr"><stx:value-of select="$title"/></stx:element><stx:text>
</stx:text>
          <stx:element name="tr"><stx:value-of select="$artist"/></stx:element><stx:text>
</stx:text>
  </stx:template>

</stx:transform>
    """
    val transFactory = new TransformerFactoryImpl()
    val transformerSource = new StreamSource(new StringReader(stxSource))

    val transformer = transFactory.newTransformer(transformerSource)



    var source : StreamSource = null


    var resultStringWriter: StringWriter = null


    def run() = {
      source = new StreamSource(new StringReader(new String(catalogSource.getBytes)))
      resultStringWriter = new StringWriter()
      val result = new StreamResult(resultStringWriter)
      transformer.transform(source, result)
      val resultString: String = resultStringWriter.toString
      //assertEquals("", resultString)
    }
    
  }
  
  class XSLTCaseBenchmark(override val nCatalog : Int = 3) extends FreetleBenchmark(nCatalog : Int) {
    override val name = "xslt"
    override def prefix = "XSLT = "
    val xsltSource = """<?xml version="1.0" encoding="UTF-8"?>
  <xsl:stylesheet version="1.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform">
  <xsl:template match="/">
    <html>
      <body>
        <xsl:for-each select="catalog/cd">
          <tr><xsl:value-of select="title"/></tr>
          <tr><xsl:value-of select="artist"/></tr>
        </xsl:for-each>
      </body>
    </html>
  </xsl:template>
  </xsl:stylesheet>
  """
    val transFactory = TransformerFactory.newInstance
    val transformerSource = new StreamSource(new StringReader(xsltSource))

    val transformer = transFactory.newTransformer(transformerSource)



    var source : StreamSource = null

    
    var resultStringWriter: StringWriter = null


    def run() = {
      source = new StreamSource(new StringReader(new String(catalogSource.getBytes)))
      resultStringWriter = new StringWriter()
      val result = new StreamResult(resultStringWriter)
      transformer.transform(source, result)
      val resultString: String = resultStringWriter.toString
      //assertEquals("", resultString) 
    }

    def checkResult() = {
      // Finished Run.
      /*val resultString: String = resultStringWriter.toString

      assertTrue("<html> not found!! [" + resultString + "]", resultString.indexOf("<html>") >= 0)
      assertTrue("</html> not found!! [" + resultString + "]", resultString.indexOf("</html>") >= 0)*/
    }
  }


  class FreetleCaseBenchmark(override val nCatalog : Int = 3) extends FreetleBenchmark(nCatalog : Int) {
    override val name = "freetle"

    def run() = FreetleCaseBenchmark.transformer.run(catalogSource = catalogSource)

    def checkResult() = {

    }
  }
  class IdentityCaseBenchmark(override val nCatalog : Int = 3) extends FreetleBenchmark(nCatalog : Int) {
    override val name = "identity"

    def run() = IdentityCaseBenchmark.transformer.run(catalogSource = catalogSource)

    def checkResult() = {
      //assertEquals("", result)
    }
  }
  object IdentityCaseBenchmark {
    val transformer : IdentityCaseBenchmarkTransform = new IdentityCaseBenchmarkTransform()
  }
  object FreetleCaseBenchmark {
    val transformer : FreetleCaseBenchmarkTransform = new FreetleCaseBenchmarkTransform()
  }

  @Test
  def testXSLT() = {
    val warmup = 4
    val testRetries = 5
    val sizes = (3 to 8).map(_ * 2500)
    //val sizes = Stream(1000)
    val benchmarks = List(
         (x :Int) => new XSLTCaseBenchmark(nCatalog = x),
         (x :Int) => new IdentityCaseBenchmark(nCatalog = x),
         (x :Int) => new STXCaseBenchmark(nCatalog = x),
         (x :Int) => new FreetleCaseBenchmark(nCatalog = x)
      )
    val benchmarksRun = benchmarks.flatMap(f => sizes.map(f))
    val timings = benchmarksRun.map(x => {
        x.catalogSource = buildRandomCatalog(x.nCatalog)
        Platform.collectGarbage()
        val r = (x.name, x.nCatalog, x.catalogSource.length, (x.runBenchmark(warmup + testRetries).drop(warmup).sum/testRetries))
        x.catalogSource = null
        r
      }).toList
    //println(timings)
    timings.foreach(
        x => {
          println(x._1 + ";" + x._2 + ";" + x._3 + ";" + x._4)
        }
      )
  }



}