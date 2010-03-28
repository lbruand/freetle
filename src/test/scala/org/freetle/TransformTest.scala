package org.freetle

import org.junit._
import Assert._
import io.Source
import org.freetle.transform._
import util.{XMLEventStream, EvEntityRef, EvProcInstr, EvComment, EvText, EvElemEnd, EvElemStart}
import scala.xml.{Atom, Unparsed, PCData, PrettyPrinter, EntityRef, ProcInstr, Comment, Text, Elem, Node, NodeSeq}

@Test
class TransformTest {
	// Because we want to test an arbitrary long length of Stream,
  // We introduce this parameter to be set arbitrarily.
 
	@Test
	def testTakeElem() = {
	  val s = Stream(new EvElemStart("p", "body", null, null)) map (Tail(_))
	  val trans = new TakeStartElement("body")
	  val r = trans(s)
	  assertEquals(1, r.length)
	  assertEquals(1, r.filter(_.isInstanceOf[Result]).length)
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
      assertEquals(2, result.filter(_.isInstanceOf[Result]).length)
    }

    /** This class is used to count the number of objects in the counter */
    class Counter(val countTotal : Int, val countResult : Int)
  
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
      assertEquals(9, r.filter(_.isInstanceOf[Result]).length)
      val t2 = new ConcatOperator(new ConcatOperator(new TakeStartElement("body"), new ConcatOperator(new TakeStartElement("body"), new TakeStartElement("a"))), new DeepFilter())
      val r2 = t2(s)
      assertEquals(6, r2.filter(_.isInstanceOf[Result]).length)
    }

     

    @Test
    def testXMLExp() = {
      val f : (String => Node) = hello => <message>{hello}</message>
      val t = new PushNode(f("hello"))
      assertEquals(3, t(Stream.empty).length)
    }

    @Test
    def testLoadXML() = {
      val src = Source.fromInputStream(this.getClass().getResourceAsStream("/org/freetle/input.xml"))
      val evStream = Stream.fromIterator( new XMLEventStream(src) map (Tail(_)) )
      val t = new SequenceOperator(new TakeStartElement("input"), new SequenceOperator(
      new RepeatUntilNoResultOperator(new SequenceOperator(new SequenceOperator(new TakeStartElement("message"),
        new SequenceOperator(new PushEvent(new EvElemStart("p", "a", null, null)), new PushEvent(new EvElemEnd("p", "a")))),
          new SequenceOperator(new DeepFilter(),new TakeEndElement("message")))), new TakeEndElement("input")))
      val r = t(evStream)
      assertEquals(20, r.length)
    }

    @Test
    def testTakeSpace() {
      val t = new TakeSpace()
      
      assertEquals(1, t(Stream(Tail(new EvElemStart("p", "a", null, null))))
              .filter(_.isInstanceOf[Result]).length)
      assertEquals(1, t(Stream(Tail(new EvText("p"))))
              .filter(_.isInstanceOf[Result]).length)
      assertEquals(1, t(Stream(Tail(new EvText("        \t        "))))
              .filter(_.isInstanceOf[Result]).length)
    }

}


