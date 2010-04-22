package org.freetle


import meta.Meta
import org.junit._
import Assert._
import io.Source

import util.{XMLEventStream, EvEntityRef, EvProcInstr, EvComment, EvText, EvElemEnd, EvElemStart}
import scala.xml.{Atom, Unparsed, PCData, PrettyPrinter, EntityRef, ProcInstr, Comment, Text, Elem, Node, NodeSeq}


class TransformTestContext {
  var name : String = null
}

@Test
class TransformTest extends TransformTestBase[TransformTestContext] with Meta[TransformTestContext] {

 
	@Test
	def testTakeElem() = {
	  val s = Stream(new EvElemStart("p", "body", null, null)) map (Tail(_, null))
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
    val input = List(new EvElemStart("p", "body", null, null), new EvElemStart("p", "message", null, null)).toStream map (Tail(_, null))
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
                    Stream.make(depthTest, new EvElemStart("p", "message", null, null)),
                    Stream(new EvElemStart("p", "body", null, null))
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
                    Stream.make(depthTest, new EvElemStart("p", "message", null, null)),
                    Stream(new EvElemStart("p", "body", null, null))
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
        new EvElemStart("p", "body", null, null),
        new EvElemStart("p", "body", null, null),
        new EvElemStart("p", "a", null, null),
        new EvElemStart("p", "a", null, null),
        new EvElemEnd("p", "a"),
        new EvElemEnd("p", "a"),
        new EvElemStart("p", "a", null, null),
        new EvElemEnd("p", "a"),
        new EvElemEnd("p", "body"),
        new EvElemEnd("p", "body")
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
    val f : (String => Node) = hello => <message>{hello}</message>
    val t = new PushNode(f("hello"))
    assertEquals(3, t(Stream.empty).length)
  }

  @Test
  def testLoadXML() = {
    val src = this.getClass().getResourceAsStream("/org/freetle/input.xml")
    val evStream = Stream.fromIterator( new XMLEventStream(src) map (Tail(_, null)) )

    val t = new TakeStartElement("input") ~
                (( new TakeStartElement("message") ~
                  new PushEvent(new EvElemStart("p", "a", null, null)) ~
                  new PushEvent(new EvElemEnd("p", "a")) ~
                  new DeepFilter() ~
                  new TakeEndElement("message")
                )+) ~ new TakeEndElement("input")
    val r = (new SpaceSkipingMetaProcessor())(t)(evStream)
    assertAllResult(r)
  }

    @Test
  def testLoadXMLWhile() = {
    val src = this.getClass().getResourceAsStream("/org/freetle/input.xml")
    val evStream = Stream.fromIterator( new XMLEventStream(src) map (Tail(_, null)) )

    val t = new TakeStartElement("input") ~
                (( new TakeStartElement("message") ~
                  new PushEvent(new EvElemStart("p", "a", null, null)) ~
                  new PushEvent(new EvElemEnd("p", "a")) ~
                  new DeepFilter() ~
                  new TakeEndElement("message")
                )*) ~ new TakeEndElement("input")
    val r = (new SpaceSkipingMetaProcessor())(t)(evStream)
    assertAllResult(r)
  }




  @Test
  def testTakeSpace() {
    val t = new TakeSpace()

    assertEquals(0, lengthResult(t(Stream(Tail(new EvElemStart("p", "a", null, null), null)))))

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

}


