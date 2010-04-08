package org.freetle


import meta.Meta
import org.junit._
import Assert._


/**
 * Created by IntelliJ IDEA.
 * User: luke
 * Date: 26 mars 2010
 * Time: 21:22:59
 * To change this template use File | Settings | File Templates.
 */

class MetaProcessorTestContext

@Test
class MetaProcessorTest extends Meta[MetaProcessorTestContext] {
  @Test
  def testRunRecursive() {
    val t = new TakeSpace()
    val tElem = new TakeStartElement("hello")
    val o = new RepeatUntilNoResultOperator(t)
    val oElem = o.clone(tElem)
    
    val m = new SpaceSkipingMetaProcessor()    
    // assertEquals(o, m(o)) TODO Find something to do with the assertion to check we are ok.
    assertTrue(true)
  }
}