/*
* Copyright 2010-2012 Lucas Bruand
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

import org.junit._
import Assert._
import util._
import java.io.StringWriter
import java.lang.String

case class TstSortContext(name :String ="name", totalSum : Int = 0, currentSum : Int = 0)

@Test
class SortTest extends CPSXMLModel[TstSortContext]  {

  @Test
  def testSort() {

    val a = """<orders><order id="2"/><order id="1"/><order id="3"/></orders>"""
    val inStream = XMLResultStreamUtils.loadXMLResultStream(a)
    val t = <("orders") ~ new SortOperator(<("order") ~ </("order"), ((new TakeAttributesToContext(new LocalPartEvStartMatcher("order")) {
      def pushToContext(name: QName, attributes: Map[QName, String], namspaces: Map[String, String], context: TstSortContext) = context.copy(name = attributes.getOrElse(new QName(localPart = "id"), ""))
    } ~ new DeepFilter()) -> drop) ~ >(c => c.name)) ~ </("orders")
    val result = t(new CFilterIdentity(),
                  /* Changing this to a FailsCFilter makes the thing go bust... Do something */new CFilterIdentity()
                  )(inStream, new TstSortContext())
    val writer = new StringWriter()
    assertTrue(!result.isEmpty)
    XMLResultStreamUtils.serializeXMLResultStream(result, writer)
    assertEquals("<orders><order id=\"1\"/><order id=\"2\"/><order id=\"3\"/></orders>", writer.toString)
  }
}
