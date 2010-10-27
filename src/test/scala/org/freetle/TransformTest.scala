package org.freetle


import meta.Meta
import org.junit._
import Assert._

import scala.xml.{Atom, Unparsed, PCData, PrettyPrinter, EntityRef, ProcInstr, Comment, Text, Elem, Node, NodeSeq}
import util._

@serializable @SerialVersionUID(599494944949L + 10000L)
class TransformTestContext {
  var name : String = null
  var totalSum : Int = 0
  var currentSum : Int = 0
}

@Test
class TransformTest extends TransformTestBase[TransformTestContext] with Meta[TransformTestContext] {
  val PREFIX : String = "p"
  val NAMESPACE : String = "http://freetle.sf.net/"
  
	@Test
	def testTakeElem() = {
	  val s = Stream(new EvElemStart(PREFIX, "body", null, null)) map (Tail(_, null))
	  val trans = new TakeStartElement("body")
	  val r = trans(s)
	  assertEquals(1, r.length)
	  assertEquals(1, lengthResult(r))
	  assertTrue("body", r.head.sub match {
	    case EvElemStart(_, "body", _, _) => true
	    case _ => false
	  })
	}
  
  @Test
  def testConcatOperator() = {
    val input = List(new EvElemStart(PREFIX, "body", null, null),
                     new EvElemStart(PREFIX, "message", null, null)).toStream map (Tail(_, null))
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
                    Stream.make(depthTest, new EvElemStart(PREFIX, "message", null, null)),
                    Stream(new EvElemStart(PREFIX, "body", null, null))
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
  def testWhileNoResultOperator() = {
    def runForIteration(depthTest :Int) = {
        val c = new WhileNoResultOperator(new TakeStartElement("message")).apply(
          (Stream.concat(
                    Stream.make(depthTest, new EvElemStart(PREFIX, "message", null, null)),
                    Stream(new EvElemStart(PREFIX, "body", null, null))
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
  def testDeep() = {

    val s = List(
        new EvElemStart(PREFIX, "body", NAMESPACE, null),
        new EvElemStart(PREFIX, "body", NAMESPACE, null),
        new EvElemStart(PREFIX, "a", NAMESPACE, null),
        new EvElemStart(PREFIX, "a", NAMESPACE, null),
        new EvElemEnd(PREFIX, "a", NAMESPACE),
        new EvElemEnd(PREFIX, "a", NAMESPACE),
        new EvElemStart(PREFIX, "a", NAMESPACE, null),
        new EvElemEnd(PREFIX, "a", NAMESPACE),
        new EvElemEnd(PREFIX, "body", NAMESPACE),
        new EvElemEnd(PREFIX, "body", NAMESPACE)
    ).toStream map (Tail(_, None))

    val t = new TakeStartElement("body") ~~ new TakeStartElement("body") ~~ new DeepFilter()
    val r = t(s)
    assertEquals(9, lengthResult(r))
    val t2 = new TakeStartElement("body") ~~ new TakeStartElement("body") ~~ new TakeStartElement("a") ~~ new DeepFilter()
    val r2 = t2(s)
    assertEquals(6, lengthResult(r2))
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
                  new PushEvent(new EvElemStart(PREFIX, "a", null, null)) ~
                  new PushEvent(new EvElemEnd(PREFIX, "a", null)) ~
                  new DeepFilter() ~
                  new TakeEndElement("message")
                )+) ~ new TakeEndElement("input")
    val r = (new SpaceSkipingMetaProcessor())(t)(evStream)
    assertAllResult(r)
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
    assertAllResult(r)
    assertEquals(evStream.length-8, r.length)
  }

  @Test
  def testLoadXMLWhile() = {
    val evStream = loadStreamFromResource("/org/freetle/input.xml")


    val t = new TakeStartElement("input") ~
                (( new TakeStartElement("message") ~
                  new PushEvent(new EvElemStart(PREFIX, "a", null, null)) ~
                  new PushEvent(new EvElemEnd(PREFIX, "a", null)) ~
                  new DeepFilter() ~
                  new TakeEndElement("message")
                )*) ~ new TakeEndElement("input")
    val r = (new SpaceSkipingMetaProcessor())(t)(evStream)
    assertAllResult(r)
  }

  @Test
  def testTakeText() {
    val t = new TakeText()
    assertEquals(1, lengthResult(t(Stream(Tail(new EvText("p"), null)))))
  }
  
  @Test
  def testTakeSpace() {
    val ev = new EvElemStart(PREFIX, "a", null, null)
    assertFalse(new EvCommentTypeMatcher()(ev))
    assertFalse(new SpaceOrCommentMatcher()(ev))
    val t = new TakeSpace()
    assertEquals("EvElemStart", 0, lengthResult(t(Stream(Tail(ev, null)))))
    assertEquals(0, lengthResult(t(Stream(Tail(new EvText("p"), null)))))
    assertEquals(1, lengthResult(t(Stream(Tail(new EvText("        \t        "), null)))))
  }

  @Test
  def testPushToContext() {
    val c = new TransformTestContext()
    c.name = "before"
    val s = List(
              new EvText("after"),
              new EvText(" night")
            ).toStream.map(x => Tail(x, Some(c)))
    val t = new TakeDataToContext() {
      def pushToContext(text : String, context : TransformTestContext) : TransformTestContext = {
          context.name = text
          context
      }
    }
    val r = t(s)
    assertEquals("after", c.name)
  }
  
  @Test
  def testOutOfContext() {
    val c = new TransformTestContext()
    c.name = "before"
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
  def testSumming() = {
    val c = new TransformTestContext()
    val evStream = mloadStreamFromResource("/org/freetle/input2.xml", Some(c))
    val totalSumTaker = new TakeDataToContext() {
      def pushToContext(text : String, context : TransformTestContext) : TransformTestContext = {
          context.totalSum = Integer.parseInt(text)
          context
      }
    }

    val sumTaker = new TakeDataToContext() {
      def pushToContext(text : String, context : TransformTestContext) : TransformTestContext = {
          context.currentSum += Integer.parseInt(text)
          context
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
    assertEquals(20030, c.totalSum)
    assertEquals(20030, c.currentSum)
  }
}


