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

case class TestXMLContext(name :String ="name", totalSum : Int = 0, currentSum : Int = 0)



@Test
class CPSXMLModelTest extends CPSXMLModel[TestXMLContext] with TestXMLHelperMethods[TestXMLContext] with CPSMeta[TestXMLContext] {
  @Test
  def testDeepFilter() {
    val s : CPSStream = List(
      createStartElem("body"),
      createStartElem("body"),
      createStartElem("a"),
      createStartElem("a"),
      createEndElem("a"),
      createEndElem("a"),
      createStartElem("a"),
      createEndElem("a"),
      createEndElem("body"),
      createEndElem("body")
      ).toStream map (x => (Some(x), false))

    val t = <("body") ~ <("body") ~ new DeepFilter()
    val r = t(filterIdentity, filterIdentity)(s, new TestXMLContext())
    assertEquals(10, lengthResult(r))
    val t2 = <("body") ~ <("body") ~ <("a") ~ new DeepFilter()
    val r2 = t2(filterIdentity, filterIdentity)(s, new TestXMLContext())
    assertEquals(serializeWithResult(r2), 9, lengthResult(r2))
  }
  
  @Test
  def testDropFilter() = {
    val evStream = loadStreamFromResource("/org/freetle/input.xml")

    val t = <("input") ~
                (( <("message") ~
                   ((<("value") ~
                   TakeText()   ~
                   </("value")) -> new DropFilter()) ~
                  </("message")
                )+) ~ </("input")
    val tmeta = t.metaProcess(new SpaceSkipingMetaProcessor())
    val r = CPSStreamHelperMethods.removeAllEmptyPositive(tmeta(filterIdentity, filterIdentity)(evStream, new TestXMLContext()))
    assertAllResult(r, "Result : [" + serializeWithResult(r) + "]" )
    assertEquals("Result : [" + serializeWithResult(r) + "]", evStream.length-8, r.length)
    assertTrue(r.filter(x => (x._1 match {
      case Some(EvElemStart(QName(_, "value", _), _)) => true
      case _  => false
    })).isEmpty)
  }

  @Test
  def testPushToContext() {
    val cfilterIdentityWithContextSuccess = new CFilterIdentityWithContext()
    val cfilterIdentityWithContextFailure = new CFilterIdentityWithContext()
    val c = new TestXMLContext(name = "before")
    val s = List(
              new EvText("after"),
              new EvText(" night")
            ).toStream.map(x => (Some(x), false))
    val t = new TakeTextToContext() {
      def pushToContext(text : String, context : TestXMLContext) : TestXMLContext = {
          context.copy(name = text)
      }
    }
    val r = t(cfilterIdentityWithContextSuccess, cfilterIdentityWithContextFailure)(s, c)
    r.force

    assertEquals("after", cfilterIdentityWithContextSuccess.context.get.name)
  }

  @Test
  def testTakeSpace() {
    val ev = createStartElem("a")
    assertFalse(new EvCommentTypeMatcher()(ev))
    assertFalse(new SpaceOrCommentMatcher()(ev))
    val t = TakeSpace()
    assertEquals("EvElemStart", 0, lengthResult(t(filterIdentity, filterIdentity)(Stream((Some(ev), false)), null)))
    assertEquals(0, lengthResult(t(filterIdentity, filterIdentity)(Stream((Some(new EvText("p")), false)), null)))
    assertEquals(1, lengthResult(t(filterIdentity, filterIdentity)(Stream((Some(new EvText("        \t        ")), false)), null)))
  }

  @Test
  def testSumming() = {
    val c = new TestXMLContext()
    val evStream = loadStreamFromResource("/org/freetle/input2.xml")
    val totalSumTaker = new TakeTextToContext() {
      def pushToContext(text : String, context : TestXMLContext) : TestXMLContext = {
          context.copy(totalSum = Integer.parseInt(text))
      }
    }

    val sumTaker = new TakeTextToContext() {
      def pushToContext(text : String, context : TestXMLContext) : TestXMLContext = {
          context.copy(currentSum = context.currentSum + Integer.parseInt(text))
      }
    }

    val t = <("input") ~
                (
                 <("groupheader") ~
                         <("totalSum") ~
                         totalSumTaker ~
                         </("totalSum") ~
                 </("groupheader")
                ) ~
                (( <("message") ~
                    <("value") ~
                         sumTaker ~
                    </("value") ~
                  </("message")
                )*) ~
            </("input")
    val tmeta = t.metaProcess(new SpaceSkipingMetaProcessor())
    val cfilterIdentityWithContextSuccess = new CFilterIdentityWithContext()
    val cfilterIdentityWithContextFailure = new CFilterIdentityWithContext()    
    val r = (tmeta)(cfilterIdentityWithContextSuccess, cfilterIdentityWithContextFailure)(evStream, c)
    assertAllResult(r)
    val cout = cfilterIdentityWithContextSuccess.context.get
    assertEquals(20030, cout.totalSum)
    assertEquals(20030, cout.currentSum)
  }  
}