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


case class TestContext(a : Int = 0)

/**
 * TODO : Explore backtracking.
 *
 * JVM Tuning : use a contrained JVM to test -Xmx10m -Xss100k in order to verify that we are streaming.
 */
@Test
class CPSStreamingVerifyTest extends CPSModel[Char, TestContext] {

  final val max = 10000
  final val filterIdentity = new CFilterIdentity()

  @Test
  def testChoiceSimple() = {
    def createStream : CPSStream = {
      Stream.continually((Some('a'), false)).take(max).append(
          Stream.continually((Some('b'), false)).take(max)
        )
    }
    val t = (new ElementMatcherTaker(_.equals('a')) | new ElementMatcherTaker(_.equals('b')))+

    t(filterIdentity, filterIdentity)(createStream, null).foreach(x => {
                                                                         assertTrue(" char : " + x._1, x._2)
                                                                       }
                                                                  )
  }

  @Test
  def testSequence() = {
    def createStream : CPSStream = {
      Stream((Some('a'), false)).append(
          Stream((Some('b'), false))
        )
    }

    val t = (new ElementMatcherTaker(_.equals('a')) ~ new ElementMatcherTaker(_.equals('b')))


    t(filterIdentity, filterIdentity)(createStream, null).foreach(x => {
                                                                         assertTrue(" char : " + x._1, x._2)
                                                                       }
                                                                  )
  }

  @Test
  def testSequenceWithZeroOrMore() = {
    def createStream : CPSStream = {
      Stream.continually((Some('a'), false)).take(max).append(
          Stream.continually((Some('b'), false)).take(max)
        )
    }

    val t = ((new ElementMatcherTaker(_.equals('a'))*) ~
                                (new ElementMatcherTaker(_.equals('b')))*)


    t(filterIdentity, filterIdentity)(createStream, null).foreach(x => {
                                                                         assertTrue(" char : " + x._1, x._2)
                                                                       }
                                                                  )
  }

  @Test
  def testSequenceWithOneOrMore() = {
    def createStream : CPSStream = {
      Stream.continually((Some('a'), false)).take(max).append(
          Stream.continually((Some('b'), false)).take(max)
        )
    }

    val t = ((new ElementMatcherTaker(_.equals('a')))+) ~
                                 ((new ElementMatcherTaker(_.equals('b')))+)

    t(filterIdentity, filterIdentity)(createStream, null).foreach(x => {
                                                                         assertTrue(" char : " + x._1, x._2)
                                                                       }
                                                                  )
  }  

  @Test
  def testTaker() = {
    def createStream : CPSStream = Stream.continually((Some('a'), false)).take(max)

    val t = (new ElementMatcherTaker(_.equals('a')))+
    
    t(filterIdentity, filterIdentity)(createStream, null).foreach(x => {
                                                                         assertTrue(x._2)
                                                                         x._1 match {
                                                                           case None => assertTrue(true)
                                                                           case Some(c) =>  assertEquals('a', c)
                                                                         }

                                                                       }
                                                                  )

    def createStreamWithFinalB : CPSStream = Stream.continually((Some('a'), false)).take(max).append(Stream((Some('b'), false)))

    t(filterIdentity, filterIdentity)(createStreamWithFinalB, null).zipWithIndex.foreach(xi => {                                                                          
                                                                         val (x, i) = xi
                                                                         i match {
                                                                           case c if (c == max) => {
                                                                             assertTrue(x._2)
                                                                             assertEquals(None, x._1)
                                                                           }
                                                                           case c if (c == max+1) => {
                                                                             assertFalse(x._2)
                                                                             assertEquals(Some('b'), x._1)
                                                                           }
                                                                           case _ => {
                                                                             assertTrue(x._2)
                                                                             assertEquals(Some('a'), x._1)
                                                                           }
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