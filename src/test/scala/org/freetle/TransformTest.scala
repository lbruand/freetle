package org.freetle

import meta.SpaceSkipingMetaProcessor
import org.junit._
import Assert._
import io.Source
import org.freetle.transform._
import util.{XMLEventStream, EvEntityRef, EvProcInstr, EvComment, EvText, EvElemEnd, EvElemStart}
import scala.xml.{Atom, Unparsed, PCData, PrettyPrinter, EntityRef, ProcInstr, Comment, Text, Elem, Node, NodeSeq}

@Test
class TransformTest {
  // Utility methods used to test XMLResultStream
  /**
   * Asserts that there are only Results in the stream.
   */
  def assertAllResult(r : XMLResultStream) :Unit = r.foreach(x => assertTrue(x.isInstanceOf[Result]))

  /**
   * Asserts that there are only Tails in the stream.
   */
  def assertAllTail(r : XMLResultStream) :Unit = r.foreach(x => assertTrue(x.isInstanceOf[Tail]))

  /**
   * Return the longest substream that begins very a Tail. 
   */
  def findFirstTail(r : XMLResultStream) : XMLResultStream = r.head match {
    case result : Result => findFirstTail(r.tail)
    case tail : Tail => r
  }

  /**
   * Constrains that after the first Tail, there is only Tails.
   */
  def constraintResultsThenTails(x : XMLResultStream) : Unit = assertAllTail(findFirstTail(x))

  /**
   * Number of Results in the Stream.
   */
  def lengthResult(r : XMLResultStream) : Int = r.filter(_.isInstanceOf[Result]).length

  /**
   * Number of Tails in the Stream.
   */
  def lengthTail(r : XMLResultStream) : Int = r.filter(_.isInstanceOf[Result]).length

 
	@Test
	def testTakeElem() = {
	  val s = Stream(new EvElemStart("p", "body", null, null)) map (Tail(_))
	  val trans = new TakeStartElement("body")
	  val r = trans(s)
	  assertEquals(1, r.length)
	  assertEquals(1, lengthResult(r))
	  assertTrue("body", r.head.sub match {
	    case EvElemStart(_, "body", _, _) => true
	    case _ => false
	  }
	  )
	}
    @Test
    def testConcatOperator() = {		        
      val input = List(new EvElemStart("p", "body", null, null), new EvElemStart("p", "message", null, null)).toStream map (Tail(_))
          val trans = new ConcatOperator(new TakeStartElement("body"), new TakeStartElement("message"))
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
                    ) map (Tail(_))))
        .foldLeft(new Counter(0,0))( (u,z) => new Counter(u.countTotal+1, u.countResult + (z match {
            case Result(_) => 1
            case _ => 0
          })) )

              assertEquals(depthTest + 1, c.countTotal)
              assertEquals(depthTest, c.countResult)


          }
      runForIteration(depthTest)
    }
    
    @Test
    def testDeep() = {
      val in = List(
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
      )
      val s = Stream.fromIterator(in.elements) map (Tail(_))
      val t = new ConcatOperator(
        new ConcatOperator(new TakeStartElement("body"), new TakeStartElement("body")),
        new DeepFilter())
      val r = t(s)
      assertEquals(9, lengthResult(r))
      val t2 = new ConcatOperator(new ConcatOperator(new TakeStartElement("body"), new ConcatOperator(new TakeStartElement("body"), new TakeStartElement("a"))), new DeepFilter())
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
      val evStream = Stream.fromIterator( new XMLEventStream(src) map (Tail(_)) )
      val t = new SequenceOperator(new TakeStartElement("input"), new SequenceOperator(
      new RepeatUntilNoResultOperator(new SequenceOperator(new SequenceOperator(new TakeStartElement("message"),
        new SequenceOperator(new PushEvent(new EvElemStart("p", "a", null, null)), new PushEvent(new EvElemEnd("p", "a")))),
          new SequenceOperator(new DeepFilter(),new TakeEndElement("message")))), new TakeEndElement("input")))
      val r = (new SpaceSkipingMetaProcessor())(t)(evStream)
      assertAllResult(r)
    }



  
    @Test
    def testTakeSpace() {
      val t = new TakeSpace()
      
      assertEquals(0, lengthResult(t(Stream(Tail(new EvElemStart("p", "a", null, null))))))
              
      assertEquals(0, lengthResult(t(Stream(Tail(new EvText("p"))))))

      assertEquals(1, lengthResult(t(Stream(Tail(new EvText("        \t        "))))))
              
    }

}


