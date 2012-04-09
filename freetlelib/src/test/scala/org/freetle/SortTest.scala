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
import util._

case class TstSortContext(name :String ="name", totalSum : Int = 0, currentSum : Int = 0)

@Test
class SortTest extends CPSModel[Char, TstSortContext]{

  val takeAnyChar = new ElementMatcherTaker(x => true)

  @Test
  def testSort() {
    val a = "zyx".toStream.map( x => (Some(x), false))
    val result = new SortOperator(takeAnyChar, takeAnyChar)(new CFilterIdentity(), new FailsCFilter())(a, new TstSortContext())
    assertEquals("xyz", (result map (_._1.get)).mkString)
  }
}
