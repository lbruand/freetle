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


import meta.Meta
import org.junit._
import Assert._

import scala.xml.{Atom, Unparsed, PCData, PrettyPrinter, EntityRef, ProcInstr, Comment, Text, Elem, Node, NodeSeq}
import util._

@serializable @SerialVersionUID(599494944949L + 10000L)
case class TransformTestContext (
  name : String = null,
  totalSum : Int = 0,
  currentSum : Int = 0
)

@Test
class TransformTest extends TransformTestBase[TransformTestContext] with Meta[TransformTestContext] {
  val PREFIX : String = "p"
  val NAMESPACE : String = "http://freetle.sf.net/"

  @Test
  def testCopyElem() = {
    val ev = createStartElem("body")
    val evCopy = ev.copy(name =  new QName(NAMESPACE, "after", PREFIX))
    assertEquals("after", evCopy.name.localPart)
  }
  
	@Test
	def testTakeElem() = {
	  val s = Stream(createStartElem("body")) map (Tail(_, null))
	  val trans = new TakeStartElement("body")
	  val r = trans(s)
	  assertEquals(1, r.length)
	  assertEquals(1, lengthResult(r))
	  assertTrue("body", r.head.subEvent match {
	    case EvElemStart(name, _) => (name.localPart.equals("body")) 
	    case _ => false
	  })
	}
  
  @Test
  def testConcatOperator() = {
    val input = List(createStartElem("body"),
                     createStartElem("message")).toStream map (Tail(_, null))
    val trans = new TakeStartElement("body") ~~ new TakeStartElement("message")
    val result = trans(input)
    assertEquals(2, result.length)
    assertEquals(2, lengthResult(result))
  }

  /** This class is used to count the number of objects in the counter */
  class Counter(val countTotal : Int, val countResult : Int)

  
  // Because we want to test an arbitrary long length of Stream,
  // We introduce this parameter to be set arbitrarily.
  val depthTest = 40000

  @Test
  def testRepeatUntilNoResultOperator() = {
    def runForIteration(depthTest :Int) = {
        val c = new RepeatUntilNoResultOperator(new TakeStartElement("message")).apply(
          (Stream.concat(
                    Stream.make(depthTest, new EvElemStart(new QName(NAMESPACE, "message", PREFIX), null)),
                    Stream(new EvElemStart(new QName(NAMESPACE, "body", PREFIX), null))
                  ) map (Tail(_, null))))
      .foldLeft(new Counter(0,0))( (u,z) => new Counter(u.countTotal+1, u.countResult + (z match {
          case Result(_, _) => 1
          case _ => 0
        })) )

            assertEquals(depthTest + 1, c.countTotal)
            assertEquals(depthTest, c.countResult)


        }
    runForIteration(depthTest)
  }

  @Test
  def testSerialize() {
    val evStream = loadStreamFromResource("/org/freetle/input.xml")
    assertEquals("""<input>
    <message pid="hello">
        <value>10010</value>
    </message>
    <message>
        <value>10010</value>
    </message>
</input>""", serialize(evStream))


  }

  @Test
  def testWhileNoResultOperator() = {
    def runForIteration(depthTest :Int) = {
        val c = new WhileNoResultOperator(new TakeStartElement("message")).apply(
          (Stream.concat(
                    Stream.make(depthTest, new EvElemStart(new QName(NAMESPACE, "message", PREFIX), null)),
                    Stream(new EvElemStart(new QName(NAMESPACE, "body", PREFIX), null))
                  ) map (Tail(_, null))))
      .foldLeft(new Counter(0,0))( (u,z) => new Counter(u.countTotal+1, u.countResult + (z match {
          case Result(_, _) => 1
          case _ => 0
        })) )

            assertEquals(depthTest + 1, c.countTotal)
            assertEquals(depthTest, c.countResult)


        }
    runForIteration(depthTest)
  }

  private def createStartElem(s : String) = new EvElemStart(new QName(NAMESPACE, s, PREFIX), null)
  private def createEndElem(s : String) = new EvElemEnd(new QName(NAMESPACE, s, PREFIX))
  @Test
  def testDeep() = {

    val s = List(
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
    ).toStream map (Tail(_, None))

    val t = new TakeStartElement("body") ~~ new TakeStartElement("body") ~~ new DeepFilter()
    val r = t(s)
    assertEquals(10, lengthResult(r))
    val t2 = new TakeStartElement("body") ~~ new TakeStartElement("body") ~~ new TakeStartElement("a") ~~ new DeepFilter()
    val r2 = t2(s)
    assertEquals(9, lengthResult(r2))
  }

  @Test
  def testXMLExp() = {
    val f : (String => Node) = variable => <message>{variable}</message>
    val t = new PushNode(f("hello"))
    assertEquals(3, t(Stream.empty).length)
  }

  @Test
  def testLoadXML() = {
    val evStream = loadStreamFromResource("/org/freetle/input.xml")

    val t = new TakeStartElement("input") ~
                (( new TakeStartElement("message") ~
                  new PushEvent(createStartElem("a")) ~
                  new PushEvent(createEndElem("a")) ~
                  new DeepFilter() ~
                  new TakeEndElement("message")
                )+) ~ new TakeEndElement("input")
    val r = (new SpaceSkipingMetaProcessor())(t)(evStream)
    assertAllResult(r, "Result : [" + serializeWithResult(r) + "]" )
  }

  @Test
  def testDropFilter() = {
    val evStream = loadStreamFromResource("/org/freetle/input.xml")

    val t = new TakeStartElement("input") ~
                (( new TakeStartElement("message") ~
                   ((new TakeStartElement("value") ~
                   (new TakeText() )  ~
                   new TakeEndElement("value")) -> new DropFilter()) ~
                  new TakeEndElement("message")
                )+) ~ new TakeEndElement("input")
    val r = (new SpaceSkipingMetaProcessor())(t)(evStream)
    assertAllResult(r, "Result : [" + serializeWithResult(r) + "]" )
    assertEquals(evStream.length-8, r.length)
    // TODO assertTrue(r.filter(_.subEvent match {}))
  }

  @Test
  def testLoadXMLWhile() = {
    val evStream = loadStreamFromResource("/org/freetle/input.xml")


    val t = new TakeStartElement("input") ~
                (( new TakeStartElement("message") ~
                  new PushEvent(createStartElem("a")) ~
                  new PushEvent(createEndElem("a")) ~
                  new DeepFilter() ~
                  new TakeEndElement("message")
                )*) ~ new TakeEndElement("input")
    val r = (new SpaceSkipingMetaProcessor())(t)(evStream)
    assertAllResult(r, "Result : [" + serializeWithResult(r) + "]" )
  }

  @Test
  def testTakeText() {
    val t = new TakeText()
    assertEquals(1, lengthResult(t(Stream(Tail(new EvText("p"), null)))))
  }
  
  @Test
  def testTakeSpace() {
    val ev = createStartElem("a")
    assertFalse(new EvCommentTypeMatcher()(ev))
    assertFalse(new SpaceOrCommentMatcher()(ev))
    val t = new TakeSpace()
    assertEquals("EvElemStart", 0, lengthResult(t(Stream(Tail(ev, null)))))
    assertEquals(0, lengthResult(t(Stream(Tail(new EvText("p"), null)))))
    assertEquals(1, lengthResult(t(Stream(Tail(new EvText("        \t        "), null)))))
  }

  @Test
  def testPushToContext() {
    val c = new TransformTestContext(name = "before")
    val s = List(
              new EvText("after"),
              new EvText(" night")
            ).toStream.map(x => Tail(x, Some(c)))
    val t = new TakeDataToContext() {
      def pushToContext(text : String, context : TransformTestContext) : TransformTestContext = {
          context.copy(name = text)
      }
    }
    val r = t(s)
    val cout = r.last.context.get
    assertEquals("after", cout.name)
  }
  
  @Test
  def testOutOfContext() {
    val c = new TransformTestContext(name = "before")
    val s = List(new EvText("after")).toStream.map(x => Tail(x, Some(c)))
    val t = new ConcatOperator(new PushFromContext() {
      def generate(context: TransformTestContext) = {
        new EvText(c.name)
      }
    }, new TakeText())
    val r = t(s)
    assertAllResult(r)
    assertEquals(2, r.length)
  }

  @Test
  def testLocationAndOffset() = {
    val c = new TransformTestContext()
    val evStream : XMLResultStream = mloadStreamFromResource("/org/freetle/input2.xml", Some(c))

    evStream.foreach( x => assertNotNull(x.subEvent.location))

    evStream.foreach( x => assertNotNull(x.subEvent.location.getCharacterOffset))

    assertTrue(evStream.foldLeft[Int](0)( (i : Int, x : TransformResult) => {
        assertTrue(i <= x.subEvent.location.getCharacterOffset)
        x.subEvent.location.getCharacterOffset
      }
    ) > 0)
  }

  @Test
  def testSumming() = {
    val c = new TransformTestContext()
    val evStream = mloadStreamFromResource("/org/freetle/input2.xml", Some(c))
    val totalSumTaker = new TakeDataToContext() {
      def pushToContext(text : String, context : TransformTestContext) : TransformTestContext = {
          context.copy(totalSum = Integer.parseInt(text))
      }
    }

    val sumTaker = new TakeDataToContext() {
      def pushToContext(text : String, context : TransformTestContext) : TransformTestContext = {
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
    val r = (new SpaceSkipingMetaProcessor())(t)(evStream)
    assertAllResult(r)
    val cout = r.last.context.get
    assertEquals(20030, cout.totalSum)
    assertEquals(20030, cout.currentSum)
  }
  


  @Test
  def testSort() = {
    val c = new TransformTestContext()
    val evStream = mloadStreamFromResource("/org/freetle/input2.xml", Some(c))
        val t = <("input") ~
                (
                 <("groupheader") ~
                         <("totalSum") ~
                         new TakeText() ~
                         </("totalSum") ~
                 </("groupheader")
                ) ~
                new SortOperator(( <("message") ~
                    <("value") ~
                         new TakeText() ~
                    </("value") ~
                  </("message")
                ), ( ((<("message")  ~
                    <("value")) -> new DropFilter()) ~
                         new TakeText() ~
                    ((</("value") ~
                  </("message")) -> new DropFilter())
                )) ~
            </("input")
    val r = (new SpaceSkipingMetaProcessor())(t)(evStream)
    assertAllResult(r)
    assertEquals("""<input>
    <groupheader>
        <totalSum>20030</totalSum>
    </groupheader>
    <message>
        <value>10010</value>
    </message>
<message>
        <value>10020</value>
    </message>
    </input>""", serialize(r))
    
  }
}


