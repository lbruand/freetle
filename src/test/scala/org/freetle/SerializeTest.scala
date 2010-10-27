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