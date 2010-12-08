package org.freetle
import org.junit._
import Assert._
/**
 * Created by IntelliJ IDEA.
 * User: caroline
 */

trait TestHelperMethods[Element, Context] extends CPSModel[Element, Context] {
  /**
   * Asserts that there are only Results in the stream.
   */
  def assertAllResult( r : CPSStream, str : String = null) :Unit = r.foreach(x => assertTrue(str, x._2))

  /**
   * Asserts that there are only Tails in the stream.
   */
  def assertAllTail(r : CPSStream) :Unit = r.foreach(x => assertTrue(!x._2))

  /**
   * Return the longest substream that begins very a Tail.
   */
  def findFirstTail(r : CPSStream) : CPSStream = if (r.head._2) findFirstTail(r.tail) else r

  /**
   * Constrains that after the first Tail, there is only Tails.
   */
  def constraintResultsThenTails(x : CPSStream) : Unit = assertAllTail(findFirstTail(x))

  /**
   * Number of Results in the Stream.
   */
  def lengthResult(r : CPSStream) : Int = r.filter(_._2).length

  /**
   * Number of Tails in the Stream.
   */
  def lengthTail(r : CPSStream) : Int = r.filter(_._2).length
  
}