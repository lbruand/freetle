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

import meta.CPSMeta
import org.junit._
import Assert._
import util._
import org.apache.log4j.{ConsoleAppender, PatternLayout, BasicConfigurator}
import java.io.StringReader

case class TstXMLContext(name :String ="name", totalSum : Int = 0, currentSum : Int = 0)



@Test
class CPSXMLModelTest extends CPSXMLModel[TstXMLContext]
                      with TestXMLHelperMethods[TstXMLContext]
                      with CPSMeta[TstXMLContext]
                      with LoggingWithConstructorLocation {
  val info : String = classOf[CPSXMLModelTest].getName
  val hello : String = {
    BasicConfigurator.resetConfiguration()
    BasicConfigurator.configure(new ConsoleAppender(new PatternLayout("%r (%l) [%t] %p %c %x - %m%n")))
    ""
  }

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
    val r = t(filterIdentity, filterIdentity)(s, new TstXMLContext())
    assertEquals(9, lengthResult(r))
    val t2 = <("body") ~ <("body") ~ <("a") ~ new DeepFilter()
    val r2 = t2(filterIdentity, filterIdentity)(s, new TstXMLContext())
    assertEquals(serializeWithResult(r2), 6, lengthResult(r2))
  }
  
  @Test
  def testDropFilter() {
    val evStream = loadStreamFromResource("/org/freetle/input.xml")

    val t = <("input") ~
                (( <("message") ~
                   ((<("value") ~
                   takeText   ~
                   </("value")) -> drop) ~
                  </("message")
                )+) ~ </("input")
    val tmeta = t.metaProcess(new SpaceSkipingMetaProcessor())
    val r = CPSStreamHelperMethods.removeAllEmptyPositive(tmeta(filterIdentity, filterIdentity)(evStream, new TstXMLContext()))
    assertAllResult(r, "Result : [" + serializeWithResult(r) + "]" )
    assertEquals("Result : [" + serializeWithResult(r) + "]", evStream.length-8, r.length)
    assertTrue(r.filter(x => (x._1 match {
      case Some(EvElemStart(QName(_, "value", _), _, _)) => true
      case _  => false
    })).isEmpty)
  }

  @Test
  def testPushToContext() {
    val cfilterIdentityWithContextSuccess = new CFilterIdentityWithContext()
    val cfilterIdentityWithContextFailure = new CFilterIdentityWithContext()
    val c = new TstXMLContext(name = "before")
    val s = List(
              new EvText("after"),
              new EvText(" night")
            ).toStream.map(x => (Some(x), false))
    val t = new TakeTextToContext() {
      def pushToContext(text : String, context : TstXMLContext) : TstXMLContext = {
          context.copy(name = text)
      }
    }
    val r = t(cfilterIdentityWithContextSuccess, cfilterIdentityWithContextFailure)(s, c)
    r.force

    assertEquals("after", cfilterIdentityWithContextSuccess.currentContext.get.name)
  }

  @Test
  def testTakeSpace() {
    val ev = createStartElem("a")
    assertFalse(new EvCommentTypeMatcher()(ev))
    assertFalse(new SpaceOrCommentMatcher()(ev))
    val t = takeSpace
    assertEquals("EvElemStart", 0, lengthResult(t(filterIdentity, filterIdentity)(Stream((Some(ev), false)), null)))
    assertEquals(0, lengthResult(t(filterIdentity, filterIdentity)(Stream((Some(new EvText("p")), false)), null)))
    assertEquals(1, lengthResult(t(filterIdentity, filterIdentity)(Stream((Some(new EvText("        \t        ")), false)), null)))
  }

  @Test
  def testChoice() {
    val evStream = loadStreamFromResource("/org/freetle/input.xml")

    val t = <("input") ~
                (( <("myMessage") ~
                   ((<("value") ~
                   takeText   ~
                   </("value")) -> drop) ~
                  </("myMessage")
                )|
                ( <("message") ~
                   ((<("value") ~
                   takeText   ~
                   </("value")) -> drop) ~
                  </("message")
                )
            +) ~ </("input")

    val tmeta = t.metaProcess(new SpaceSkipingMetaProcessor())
    val r = CPSStreamHelperMethods.removeAllEmptyPositive(tmeta(filterIdentity, filterIdentity)(evStream, new TstXMLContext()))
    assertAllResult(r, "Result : [" + serializeWithResult(r) + "]" )
    assertEquals("Result : [" + serializeWithResult(r) + "]", evStream.length-8, r.length)
    assertTrue(r.filter(x => (x._1 match {
      case Some(EvElemStart(QName(_, "value", _), _, _)) => true
      case _  => false
    })).isEmpty)
  }

  @Test
  def testEmptyString() {
    logger.info("testEmptyString")
    val c = new TstXMLContext()
    val input = """
<input>
    <text></text>
</input>


"""
    val t = <("input") ~ <("text") ~ takeText ~ </("text") ~ </("input")
    val (_, _, r) = runTransform(t, input, c)
    assertAllResult(r)
  }

  @Test
  def testTakeTextHello() {
    logger.info("testTakeTextHello")
    val c = new TstXMLContext()
    val input = """
<input>
    <text>   hello  </text>
</input>


"""
    val t = <("input") ~ <("text") ~ takeText ~ </("text") ~ </("input")
    val (_, _, r) = runTransform(t, input, c)
    assertAllResult(r)
  }

  @Test
  def testTakeTextHelloToContext() {
    logger.info("testTakeTextHelloToContext")
    val c = new TstXMLContext()
    val input = """
<input>
    <text>   hello  </text>
</input>


"""
    val t = <("input") ~ <("text") ~ new TakeTextToContext {
      def pushToContext(text: String, context: TstXMLContext) = context.copy(name = text)
    } ~ </("text") ~ </("input")
    val (suc, _, r) = runTransform(t, input, c)
    assertAllResult(r)
    assertEquals("   hello  ", suc.get.name)
  }

  @Test
  def testTakeTextEmptyToContext() {
    logger.info("testTakeTextEmptyToContext")
    val c = new TstXMLContext()
    val input = """
<input>
    <text></text>
</input>


"""
    val t = <("input") ~ <("text") ~ new TakeTextToContext {
      def pushToContext(text: String, context: TstXMLContext) = context.copy(name = text)
    } ~ </("text") ~ </("input")
    val (suc, _, r) = runTransform(t, input, c)
    assertAllResult(r)
    assertEquals("", suc.get.name)
  }

  @Test
  def testTakeTextSecondEmptyToContext() {
    logger.info("testTakeTextSecondEmptyToContext")
    val c = new TstXMLContext()
    val input = """
<input>
    <text/>
</input>


"""
    val t = <("input") ~ <("text") ~ new TakeTextToContext {
      def pushToContext(text: String, context: TstXMLContext) = context.copy(name = text)
    } ~ </("text") ~ </("input")
    val (suc, _, r) = runTransform(t, input, c)
    assertAllResult(r)
    assertEquals("", suc.get.name)
  }

  @Test
  def testTakeTextWhiteSpaceToContext() {
    logger.info("testTakeTextWhiteSpaceToContext")
    val c = new TstXMLContext()
    val input = """
<input>
    <text>  </text>
</input>


"""
    val t = <("input") ~ <("text") ~ new TakeTextToContext {
      def pushToContext(text: String, context: TstXMLContext) = context.copy(name = text)
    } ~ </("text") ~ </("input")
    val (suc, _, r) = runTransform(t, input, c)
    assertAllResult(r)
    assertEquals("  ", suc.get.name)
  }

  @Test
  def testNoTagText() {
    logger.info("testNoTagText")
    val c = new TstXMLContext()
    val input = """
<input>
</input>


"""
    val t = <("input") ~ <("text") ~ takeText ~ </("text") ~ </("input")
    val (_, _, r) = runTransform(t, input, c)
    constraintResultsThenTails(r)

  }


  def runTransform(t : ChainedTransformRoot, input : String, c : TstXMLContext) = {
    val evStream = XMLResultStreamUtils.loadXMLResultStream(input)
    val tmeta = t.metaProcess(new SpaceSkipingMetaProcessor())
    val cfilterIdentityWithContextSuccess = new CFilterIdentityWithContext()
    val cfilterIdentityWithContextFailure = new CFilterIdentityWithContext()
    val result = (tmeta)(cfilterIdentityWithContextSuccess, cfilterIdentityWithContextFailure)(evStream, c)
    result.force
    (cfilterIdentityWithContextSuccess.currentContext, cfilterIdentityWithContextFailure.currentContext, result)
  }

  @Test
  def testSumming() {



    logger.info("testSumming")
    val c = new TstXMLContext()
    val evStream = loadStreamFromResource("/org/freetle/input2.xml")
    val totalSumTaker = new TakeTextToContext() {
      def pushToContext(text : String, context : TstXMLContext) : TstXMLContext = {
          context.copy(totalSum = Integer.parseInt(text))
      }
    }

    val sumTaker = new TakeTextToContext() {
      def pushToContext(text : String, context : TstXMLContext) : TstXMLContext = {
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
    val cout = cfilterIdentityWithContextSuccess.currentContext.get
    assertEquals(20030, cout.totalSum)
    assertEquals(20030, cout.currentSum)
  }

  @Test
  def testPushNode() {
    val p = new PushNode(x => x match {
      case Some(a) => <hello v="helloA">hello</hello>
      case None => <hello/>
    })
    val resultS = p.apply(new CFilterIdentity(), new CFilterIdentity())(Stream.empty, new TstXMLContext())
    assertEquals("helloA", resultS.head._1 match {
      case Some(EvElemStart(name, attrs, namespcs)) => attrs.head._2
      case _ => "error"
    })
  }
}
