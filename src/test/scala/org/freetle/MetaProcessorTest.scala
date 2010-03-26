package org.freetle

import meta.RecursiveMetaProcessor
import org.junit._
import Assert._
import org.freetle.transform._

/**
 * Created by IntelliJ IDEA.
 * User: luke
 * Date: 26 mars 2010
 * Time: 21:22:59
 * To change this template use File | Settings | File Templates.
 */
@Test
class MetaProcessorTest {
  @Test
  def testRunRecursive() {
    val t = new TakeSpace()
    val tElem = new TakeStartElement("hello")
    val o = new RepeatUntilNoResultOperator(t)
    val oElem = o.clone(tElem)
    
    val m = new RecursiveMetaProcessor() {
      def map(in: BaseTransform) = {
        val takeSpc = new RepeatUntilNoResultOperator(new TakeSpace())
        new SequenceOperator(new SequenceOperator(takeSpc, in), takeSpc)
      }
    }

    assertEquals(t, m(o))
  }
}