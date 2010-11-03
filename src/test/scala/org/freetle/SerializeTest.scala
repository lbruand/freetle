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
import java.io.{ByteArrayInputStream, ObjectOutputStream, ByteArrayOutputStream}

/**
 * Testing serialisability
 */
@Test
class SerializeTest extends TransformTestBase[TransformTestContext] with Meta[TransformTestContext] {
  @Test
  def testSerialize() {


    
    val t = new SpaceSkipingMetaProcessor() (<("input") ~
          (
           <("groupheader") ~
                   <("totalSum") ~
                   </("totalSum") ~
           </("groupheader")
          ) ~
          (( <("message") ~
              <("value") ~
              </("value") ~
            </("message")
          )*) ~
      </("input"))
    val bout = new java.io.ByteArrayOutputStream()
    val objStr = new java.io.ObjectOutputStream(bout)
    objStr.writeObject(t)
    objStr.close()
    bout.close()
    val ba = bout.toByteArray
    assertEquals(4518, ba.size)
    val inStr = new java.io.ObjectInputStream(new ByteArrayInputStream(ba))
    val rt = inStr.readObject()

    assertTrue(rt.isInstanceOf[CFilterBase])
    
  }

}