package org.freetle

import meta.Meta
import java.io.ObjectOutputStream
import java.io.ByteArrayOutputStream
import org.junit._
import Assert._

/**
 * Testing serialisability
 */
@Test
class SerializeTest extends TransformTestBase[TransformTestContext] with Meta[TransformTestContext] {
  @Test
  def testSerialize() {


    
    val t = <("input") ~
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
      </("input")
    val bout = new java.io.ByteArrayOutputStream()
    val objStr = new java.io.ObjectOutputStream(bout)
    objStr.writeObject(t)
    objStr.close()
    
  }

}