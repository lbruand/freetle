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

import org.junit._
import Assert._
import util.{Memoize1, XMLEventStream}
import java.io.InputStream

class TransformTestBase[Context] extends TransformModel[Context] {

  def mloadStreamFromResource(resourceName: String, context : Option[Context]): XMLResultStream = {
    val src: InputStream = this.getClass().getResourceAsStream(resourceName)
    Stream.fromIterator(new XMLEventStream(src) map (Tail(_, context)))
  }
  def sloadStreamFromResource(resourceName: String) = mloadStreamFromResource(resourceName, null)
  def loadStreamFromResource = new Memoize1(sloadStreamFromResource)
  // Utility methods used to test XMLResultStream
  /**
   * Asserts that there are only Results in the stream.
   */
  def assertAllResult( r : XMLResultStream, str : String = null) :Unit = r.foreach(x => assertTrue(str, x.isInstanceOf[Result]))

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

  /**
   * Serialize ( not very efficient ).
   */
  def serialize(x : XMLResultStream) : String =
    (new StringBuilder()).appendAll(XMLResultStreamUtils.serializeXMLResultStream(x)).toString

   /**
   * Serialize ( not very efficient ).
   */
  def serializeWithResult(x : XMLResultStream) : String = {
    val charStream : Stream[Char] = (x map
            (y => Stream.cons(if (y.isInstanceOf[Result]) 'R' else 'T', y.subEvent.toStream))).flatten
    charStream.toString
  }
}