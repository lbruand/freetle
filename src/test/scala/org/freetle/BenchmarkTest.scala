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

abstract class BaseCaseBenchmarkTransform
          extends CPSXMLModel[FreetleCaseBenchmarkContext]
              with CPSMeta[FreetleCaseBenchmarkContext] {

    //val transform = new CFilterIdentity()
    val transform : CFilter
  
    def run(catalogSource : String) : String = {

      val context = new FreetleCaseBenchmarkContext()
      val inStream = XMLResultStreamUtils.loadXMLResultStream(catalogSource)
      val outStream = transform(inStream, context)
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
    val name : String
    val catalogSource : String = buildRandomCatalog(nCatalog)
    
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

    def run() = FreetleCaseBenchmark.transformer.run(catalogSource = catalogSource)

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
    //val sizes = (2 to 4).map(_ * 2500)
    val sizes = List(5000)
    val benchmarks = List.concat(
                    (sizes).map(x => new FreetleCaseBenchmark(nCatalog = x))
                    ,
                    (sizes).map(x => new IdentityCaseBenchmark(nCatalog = x))
                    ,
                    (sizes).map(x => new XSLTCaseBenchmark(nCatalog = x))
                  )
    println(benchmarks.head.catalogSource.length)
    val timings = benchmarks.map(x => (x.name, x.nCatalog, x.runBenchmark(5)))


    println(timings)

  }
  
}