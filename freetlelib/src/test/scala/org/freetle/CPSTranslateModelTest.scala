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
import org.junit._
import Assert._
import util.{EvText, EvElemStart, QName}

case class CPSTranslateModelTstContext(value : String)
/**
 * Testing the model translation.
 */
@Test
class CPSTranslateModelTest extends CPSTranslateModel[CPSTranslateModelTstContext] {
  @Test
  def testPositiveCase() {
    val takeValueToContext = new TakeResultToContext {
      def pushToContext(text: String, context: CPSTranslateModelTstContext) = context.copy(value = text)
    }
    val input = """::header56
::flatfile
::footer55
""".toStream map ( (x : Char) => (Some(Left(x)), false))
    val t = (((const("::") ~ ((repeat(takeAnyChar, 8) -> takeValueToContext) ~ const("\n")) ->
      (drop ~ >((c : CPSTranslateModelTstContext) => Stream(EvText(c.value))) )  )*))

    //
    t(new CFilterIdentity(), new CFilterIdentity())(input, new CPSTranslateModelTstContext("")) foreach (x => assertTrue(x._2))
  }

  @Test
  def testNegativeCase() {
    val takeValueToContext = new TakeResultToContext {
      def pushToContext(text: String, context: CPSTranslateModelTstContext) = context.copy(value = text)
    }
    val input = """<>header56
::flatfile
::footer55
""".toStream map ( (x : Char) => (Some(Left(x)), false))
    val t = (((const("::") ~ ((repeat(takeAnyChar, 8) -> takeValueToContext) ~ const("\n")) ->
      (drop ~ >((c : CPSTranslateModelTstContext) => Stream(EvText(c.value))) )  )*))

    val result = t(new CFilterIdentity(), new CFilterIdentity())(input, new CPSTranslateModelTstContext(""))
    CPSStreamHelperMethods.removeAllEmptyPositive(result) foreach (x => assertFalse(x._2))
  }

  @Test
  def testTakeADigit() {
    assertEquals(Stream.Empty, Stream.continually(0).take(0))
    val input = "01234567899".toStream map ( (x : Char) => (Some(Left(x)), false))
    (takeADigit+)(new CFilterIdentity(), new CFilterIdentity())(input, null) foreach (x => assertTrue(x._2))
  }

}