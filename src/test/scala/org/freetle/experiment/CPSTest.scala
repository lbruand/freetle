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
package org.freetle.experiment
import org.junit._
import Assert._
import org.freetle.CPSModel


case class TestContext(a : Int = 0)

@Test
class CPSTest extends CPSModel[Char, TestContext] {

  val max = 100000


  @Test
  def testTaker() = {
    def createStream : CPSStream = Stream.continually((Some('a'), false)).take(max)

    val t = new RepeatingOperator(new Taker(_.equals('a')))
    val filterIdentity = new CFilterIdentity()
    
    t(filterIdentity, filterIdentity)(createStream, null).foreach(x => {
                                                                         assertTrue(x._2)
                                                                         assertEquals(Some('a'), x._1)
                                                                       }
                                                                  )
    def createStreamWithFinalB : CPSStream = Stream.continually((Some('a'), false)).take(max).append(Stream((Some('b'), false)))

    t(filterIdentity, filterIdentity)(createStreamWithFinalB, null).zipWithIndex.foreach(xi => {                                                                          
                                                                         val (x, i) = xi 
                                                                         if (i != max) {
                                                                           assertTrue(x._2)
                                                                           assertEquals(Some('a'), x._1)
                                                                         } else {
                                                                           assertFalse(x._2)
                                                                           assertEquals(Some('b'), x._1)
                                                                         }
                                                                       }
                                                                  )

  }
  @Test
  def testStream() = {
    def recurse(s : CPSStream, context : TestContext) : CPSStream = {
      if (s.isEmpty)
        s
      else
        if (s.head._1.get.equals('a'))
          Stream.cons((Some('b'), true), recurse(s.tail,
                                    (if (context != null) context.copy(a = context.a +1) else null)
                      ))
        else
          s
    }
    (recurse(
      Stream.continually((Some('a'), false)).take(max), null)).foreach(x => assertEquals(Some('b'), x._1))
  }

  @Test
  def testStreamSequence() = {
    def recurse(s : CPSStream, context : TestContext) : CPSStream = {
      if (s.isEmpty)
        s
      else
        if (s.head._1.get.equals('a'))
          Stream.cons((Some('b'), true), recurse(s.tail,
                                    (if (context != null) context.copy(a = context.a +1) else null)
                      ))
        else
          s
    }
    
    (recurse(
      Stream.continually((Some('a'), false)).take(max).append(Stream((Some('b'), false))), null)).foreach(x => assertEquals(Some('b'), x._1))
  }

  @Test
  def testStreamContext() = {
      def recurse(s : CPSStream, context : TestContext) : CPSStream = {
        if (s.isEmpty)
          s
        else
          if (s.head._1.get.equals('a'))
            Stream.cons((Some('b'), true), recurse(s.tail,
                                      (if (context != null) context.copy(a = context.a +1) else null)
                        ))
          else
            s
      }
      
    (recurse(
      Stream.continually((Some('a'), false)).take(max).append(Stream((Some('b'), false))), new TestContext(a=0))).foreach( x => assertEquals(Some('b'), x._1))
  }

}