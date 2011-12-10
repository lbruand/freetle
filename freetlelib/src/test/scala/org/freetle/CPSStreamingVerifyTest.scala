 /*
  * Copyright 2010-2011 Lucas Bruand
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


case class TstCPSStreamingContext(a : Int = 0)

/**
 * These tests are asserting the capability of the transformation to work in streaming mode.
 * TODO : Explore backtracking.
 *
 * JVM Tuning : use a contrained JVM to test -Xmx10m -Xss100k in order to verify that we are streaming.
 */
@Test
class CPSStreamingVerifyTest extends CPSModel[Char, TstCPSStreamingContext] {

  final val max = 10000
  final val filterIdentity = new CFilterIdentity()


  
  @Test
  def testCompose() {
    val localMax = max.max(1000) // This is to prevent OutOfMemory problems.
    def createStream : CPSStream = {
      Stream.continually((Some('a'), false)).take(localMax).append(
          Stream.continually((Some('b'), false)).take(localMax)
        )
    }
    val s = createStream
    val t = ((new ElementMatcherTaker(_.equals('a'))*) ~ (new ElementMatcherTaker(_.equals('b')))*) -> drop
    val r = t(filterIdentity, filterIdentity)(createStream, new TstCPSStreamingContext())
    assertTrue(""+r, CPSStreamHelperMethods.isOnlyEmptyPositiveStream(r))

  }
  @Test
  def testChoiceSimple() {
    def createStream : CPSStream = {
      Stream.continually((Some('a'), false)).take(max).append(
          Stream.continually((Some('b'), false)).take(max)
        )
    }
    val t = ((new ElementMatcherTaker(_.equals('a')) | new ElementMatcherTaker(_.equals('b')))+)

    t(filterIdentity, filterIdentity)(createStream, null).foreach(x => {
                                                                         assertTrue(" char : " + x._1, x._2)
                                                                       }
                                                                  )
  }

  @Test
  def testChoiceBacktracking() {
    def createStream : CPSStream = {
      Stream.continually(("aaab".toStream map (x => (Some(x), false)))).take(max).flatten
    }
    /*
     This transformation does not recognize the previous stream because this is no backtracking coded in the
     sequence operator.
     */
    val t = (((
              new ElementMatcherTaker(_.equals('a'))
              ~ new ElementMatcherTaker(_.equals('a'))
              ~ new ElementMatcherTaker(_.equals('b'))
            ) |
            (
              new ElementMatcherTaker(_.equals('a'))
              ~ new ElementMatcherTaker(_.equals('a'))
              ~ new ElementMatcherTaker(_.equals('a'))
              ~ new ElementMatcherTaker(_.equals('b'))
            ))*)

    assertTrue(t(filterIdentity, filterIdentity)(createStream, null).exists(p => {
      val (x, y) = p
      Some('b').equals(x) && y == false
    }))
  }

  @Test
  def testChoiceBacktrackingWithAdvanced() {
    def createStream : CPSStream = {
      Stream.continually(("aaab".toStream map (x => (Some(x), false)))).take(max).flatten
    }
    /*
     This transformation does recognize the previous stream because this is no need for backtracking coded in the
     sequence operator.
     */
    val t = (((
              new ElementMatcherTaker(_.equals('b'))
              ~ new ElementMatcherTaker(_.equals('a'))
              ~ new ElementMatcherTaker(_.equals('b'))
            ) |
            (
              new ElementMatcherTaker(_.equals('a'))
              ~ new ElementMatcherTaker(_.equals('a'))
              ~ new ElementMatcherTaker(_.equals('a'))
              ~ new ElementMatcherTaker(_.equals('b'))
            ))*)

    t(filterIdentity, filterIdentity)(createStream, null).foreach(x => {
                                                                         assertTrue(" char : " + x._1, x._2)
                                                                       }
                                                                  )
  }

  @Test
  def testChoiceNoBacktracking() {
    def createStream : CPSStream = {
      Stream.continually(("aaab".toStream map (x => (Some(x), false)))).take(max).flatten
    }

    val t = (((
              new ElementMatcherTaker(_.equals('a'))
              ~ new ElementMatcherTaker(_.equals('a'))
              ~ new ElementMatcherTaker(_.equals('a'))
              ~ new ElementMatcherTaker(_.equals('b'))
            ) |
            (
              new ElementMatcherTaker(_.equals('a'))
              ~ new ElementMatcherTaker(_.equals('a'))
              ~ new ElementMatcherTaker(_.equals('b'))
            ))*)
    
    t(filterIdentity, filterIdentity)(createStream, null).foreach(x => {
                                                                         assertTrue(" char : " + x._1, x._2)
                                                                       }
                                                                  )
  }

  @Test
  def testSequence() {
    def createStream : CPSStream = {
      Stream((Some('a'), false)).append(
          Stream((Some('b'), false))
        )
    }

    val t = (new ElementMatcherTaker(_.equals('a')) ~ new ElementMatcherTaker(_.equals('b')))


    t(filterIdentity, filterIdentity)(createStream, null).
            foreach(x => {
                assertTrue(" char : " + x._1, x._2)
              }
            )
  }

  @Test
  def testSequenceWithZeroOrMore() {
    def createStream : CPSStream = {
      Stream.continually((Some('a'), false)).take(max).append(
          Stream.continually((Some('b'), false)).take(max)
        )
    }

    val t = ((new ElementMatcherTaker(_.equals('a'))*) ~
                                (new ElementMatcherTaker(_.equals('b')))*)


    t(filterIdentity, filterIdentity)(createStream, null).
            foreach(x => {
                assertTrue(" char : " + x._1, x._2)
              }
            )
  }

  @Test
  def testSequenceWithOneOrMore() {
    def createStream : CPSStream = {
      Stream.continually((Some('a'), false)).take(max).append(
          Stream.continually((Some('b'), false)).take(max)
        )
    }

    val t = ((new ElementMatcherTaker(_.equals('a')))+) ~
                                 ((new ElementMatcherTaker(_.equals('b')))+)

    t(filterIdentity, filterIdentity)(createStream, null).
            foreach(x => {
                   assertTrue(" char : " + x._1, x._2)
                 }
            )
  }

  /**
   * Prove how one can count the number of occurrence of a rule.
   */
  @Test
  def testCounting() {
    def createStream : CPSStream = Stream.continually((Some('a'), false)).take(max).append(Stream((Some('b'), false)))
    val t = ((new ElementMatcherTaker(_.equals('a')) -> new ContextWritingTransform {
      def metaProcess(metaProcessor : MetaProcessor) =
              metaProcessor.processTransform(this, () => { this })
       def apply(s : CPSStream, c : TstCPSStreamingContext) : (CPSStream, TstCPSStreamingContext) = {
         (s, c.copy(a = c.a+1))
       }
    })+ ) ~ new ElementMatcherTaker(_.equals('b'))

    val filterIdentityWithContext = new CFilterIdentityWithContext();
    t(filterIdentityWithContext, filterIdentityWithContext)(createStream, new TstCPSStreamingContext(a = 0)).foreach(x => assertTrue(true))
    assertEquals(Some(TstCPSStreamingContext(a = max)), filterIdentityWithContext.context)

  }

  @Test
  def testTaker() {
    def createStream : CPSStream = Stream.continually((Some('a'), false)).take(max)

    val t = ((new ElementMatcherTaker(_.equals('a')))+)

    t(filterIdentity, filterIdentity)(createStream, null).foreach(x => {
           assertTrue(x._2)
           x._1 match {
             case None => assertTrue(true)
             case Some(c) =>  assertEquals('a', c)
           }

         }
    )

    def createStreamWithFinalB : CPSStream = Stream.continually((Some('a'), false)).take(max).append(Stream((Some('b'), false)))

    t(filterIdentity, filterIdentity)(createStreamWithFinalB, null).zipWithIndex.foreach(
      xi => {
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
  
}