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
import org.junit._
import Assert._
/**
 * Test helper class.
 */

trait TestHelperMethods[Element, Context] extends CPSModel[Element, Context] {
  /**
   * Asserts that there are only Results in the stream.
   */
  def assertAllResult( r : CPSStream, str : String = null) {
    r.foreach(x => assertTrue(str, x._2))
  }

  /**
   * Asserts that there are only Tails in the stream.
   */
  def assertAllTail(r : CPSStream) {
    r.foreach(x => assertTrue(!x._2))
  }

  /**
   * Return the longest substream that begins very a Tail.
   */
  def findFirstTail(r : CPSStream) : CPSStream = if (r.head._2) findFirstTail(r.tail) else r

  /**
   * Constrains that after the first Tail, there is only Tails.
   */
  def constraintResultsThenTails(x : CPSStream) {
    assertAllTail(findFirstTail(x))
  }

  /**
   * Number of Results in the Stream.
   */
  def lengthResult(r : CPSStream) : Int = r.filter(_._2).length

  /**
   * Number of Tails in the Stream.
   */
  def lengthTail(r : CPSStream) : Int = r.filter(_._2).length
  
}