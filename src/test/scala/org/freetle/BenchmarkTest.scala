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
package org.freetle

import meta.CPSMeta
import org.junit._
import Assert._
import util._
import javax.xml.transform.TransformerFactory
import javax.xml.transform.stream.{StreamResult, StreamSource}
import java.io.{StringWriter, StringReader}
import testing.Benchmark


@serializable @SerialVersionUID(599494944949L + 30000L)
case class FreetleCaseBenchmarkContext(title : String = null, artist : String = null)

/**
 * A test to benchmark the performance of the system.
 */
@Test
class BenchmarkTest {
  val catalogHeader = """<?xml version="1.0" encoding="UTF-8"?>
<catalog>""".toStream

  val catalogFooter = """</catalog>
""".toStream

  def buildCD(title : String, artist : String, country : String, company : String, price : String, year : String) : String = {
    var sb = new StringBuilder()
    sb.append("<cd>\n")

    sb.append("<title>")
    sb.append(title)
    sb.append("</title>\n")

    sb.append("<artist>")
    sb.append(artist)
    sb.append("</artist>\n")

    sb.append("<country>")
    sb.append(country)
    sb.append("</country>\n")

    sb.append("<company>")
    sb.append(company)
    sb.append("</company>\n")

    sb.append("<price>")
    sb.append(price)
    sb.append("</price>\n")

    sb.append("<year>")
    sb.append(year)
    sb.append("</year>\n")

    sb.append("</cd>\n")
    sb.toString
  }
  val random = new RandomUtils()
  def buildRandomCD() : String = {
    buildCD("the " + random.nextStringLetterOrDigit(22) + " album",
            random.nextStringLetterOrDigit(15) + " and the band",
            random.nextStringLetterOrDigit(2),
            random.nextStringLetterOrDigit(15) + " inc.",
            "" + (random.nextInt(2000) * 1.0 / 100),
            "" + (1950 + random.nextInt(60))
        )
  }
  def buildRandomCatalog(n : Int) : String = {
    Stream.concat(catalogHeader,
                  Stream.fill(n)(buildRandomCD().toStream).flatten,
                  catalogFooter).mkString
  }
  abstract class FreetleBenchmark(val nCatalog : Int = 3) extends Benchmark {
    val catalogSource : String = buildRandomCatalog(nCatalog)
    
  }

  
  class XSLTCaseBenchmark(override val nCatalog : Int = 3) extends FreetleBenchmark(nCatalog : Int) {
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
    var result : StreamResult = null

    def run() = {
      source = new StreamSource(new StringReader(new String(catalogSource.getBytes)))
      resultStringWriter = new StringWriter()
      result = new StreamResult(resultStringWriter)
      transformer.transform(source, result)
    }

    def checkResult() = {
      // Finished Run.
      val resultString: String = resultStringWriter.toString

      assertTrue("<html> not found!! [" + resultString + "]", resultString.indexOf("<html>") >= 0)
      assertTrue("</html> not found!! [" + resultString + "]", resultString.indexOf("</html>") >= 0)
    }
  }



  class FreetleCaseBenchmark(override val nCatalog : Int = 3) extends FreetleBenchmark(nCatalog : Int) {


    
    class FreetleCaseBenchmarkTransform
            extends CPSXMLModel[FreetleCaseBenchmarkContext]
                with CPSMeta[FreetleCaseBenchmarkContext] {
      //def transform : CFilterBase = new IdTransform()
      /**
       * Real transform construction.
       */
      def transform : ChainedTransformRoot = {
        
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

        val t = (<("catalog") -> new DropFilter()
                )~
                    (((( (<("cd") ~
                        <("title") ~
                            titleTaker ~
                        </("title") ~
                        <("artist") ~
                            artistTaker ~
                        </("artist") ~
                        <("country") ~
                             TakeText() ~
                        </("country") ~
                        <("company") ~
                             TakeText() ~
                        </("company") ~
                        <("price") ~
                             TakeText() ~
                        </("price") ~
                        <("year") ~
                             TakeText() ~
                        </("year") ~
                      </("cd"))  -> new DropFilter()) ~ new PushNode(
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
                (</("catalog") -> new DropFilter()
                        )
        t.metaProcess(new SpaceSkipingMetaProcessor())
      }

      def run(catalogSource : String) : String = {
        val filterIdentity = new CFilterIdentity()
        val context = new FreetleCaseBenchmarkContext()
        val inStream = XMLResultStreamUtils.loadXMLResultStream(catalogSource, Some(context))
        val outStream = transform(filterIdentity, filterIdentity)(inStream, context)
        result = new StringBuilder().appendAll(XMLResultStreamUtils.serializeXMLResultStream(outStream)).toString
        result
      }
      
    }
    val transformer : FreetleCaseBenchmarkTransform = new FreetleCaseBenchmarkTransform()
    var result : String = null
    
    def run() = transformer.run(catalogSource = catalogSource)

    def checkResult() = {
      //assertEquals("", result)
    }
  }

  @Test
  def testXSLT() = {
    val freetleBenchmark = new FreetleCaseBenchmark(nCatalog = 1000)
    val benchmarks = List( new XSLTCaseBenchmark(nCatalog = 1000),
                          freetleBenchmark)
    val timings = benchmarks.map(_.runBenchmark(5))

    freetleBenchmark.checkResult

    println(timings)

  }
  
}