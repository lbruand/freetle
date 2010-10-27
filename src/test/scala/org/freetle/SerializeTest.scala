package org.freetle

import meta.Meta
import java.io.ObjectOutputStream
import java.io.ByteArrayOutputStream
import org.junit._
import Assert._

/**
 * Created by IntelliJ IDEA.
 * User: luke
 * Date: 22 oct. 2010
 * Time: 21:35:30
 * To change this template use File | Settings | File Templates.
 */
@Test
@serializable
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