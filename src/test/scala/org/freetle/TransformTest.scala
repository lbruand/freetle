package org.freetle

import org.junit._
import Assert._
import scala.xml.{Node,NodeSeq}
import scala.xml.PrettyPrinter
import util.{EvElemEnd, EvElemStart}
import java.io.InputStreamReader
import io.Source
import org.freetle.util.XMLEventStream
import org.freetle.transform._

@Test
class TransformTest {
	// Because we want to test on arbitrary long length of Stream,
    // We introduce this parameter to be set arbitrarily.
 
	val depthTest = 40000
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
      val t = new ConcatOperator(new ConcatOperator(new TakeStartElement("body"), new TakeStartElement("body")), new DeepFilter())
      val r = t(s)
      assertEquals(9, r.filter(_.isInstanceOf[Result]).length)
      val t2 = new ConcatOperator(new ConcatOperator(new TakeStartElement("body"), new ConcatOperator(new TakeStartElement("body"), new TakeStartElement("a"))), new DeepFilter())
      val r2 = t2(s)
      assertEquals(6, r2.filter(_.isInstanceOf[Result]).length)
    }
    
    @Test
    def testXMLExp() = {
      val f : (String => Node) = hello => <message>{hello}</message>
      val pp = new PrettyPrinter(80, 5);

      val r = pp.format(f("h"))
      assertEquals("<message>h</message>", r)     
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
      r.foreach(println)
      
    }

}


