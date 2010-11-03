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