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
package org.freetle.util

import org.junit._
import Assert._
import org.freetle.{TestXMLHelperMethods, CPSXMLModel}
import java.io._

/**
 * Context specific to this test.
 */
case class XMLEventTestContext

/**
 * Testing what is specific to XMLEvents and related functions.
 */
@Test
class XMLEventTest extends CPSXMLModel[XMLEventTestContext] with TestXMLHelperMethods[XMLEventTestContext] {
  @Test
  def testLocationAndOffset() = {
    val c = null
    val evStream : XMLResultStream = mloadStreamFromResource("/org/freetle/input2.xml", Some(c))

    evStream.foreach( x => assertNotNull(x._1.get.location))

    evStream.foreach( x => assertNotNull(x._1.get.location.getCharacterOffset))

    assertTrue(evStream.foldLeft[Int](0)( (i : Int, x : (Option[XMLEvent], Boolean)) => {
        assertTrue(i <= x._1.get.location.getCharacterOffset)
        x._1.get.location.getCharacterOffset
      }
    ) > 0)
  }

  @Test
  def testSerialize() {
    val evStream = loadStreamFromResource("/org/freetle/input.xml")
    assertEquals("""<input>
    <message pid="hello">
        <value>10010</value>
    </message>
    <message>
        <value>10010</value>
    </message>
</input>""", serialize(evStream))
  }
  def rehydrate(in : ObjectInputStream) : XMLResultStream = {
    val read = in.readObject
    if (read != null) {
      Stream.cons( (Some((read).asInstanceOf[XMLEvent]), false), rehydrate(in))
    } else {
      Stream.Empty
    }
  }

  def dehydrate(evStream: XMLResultStream, dataOut: ObjectOutputStream): Unit = {
    evStream.foreach(x => {dataOut.writeObject(x._1.get)})
  }

  @Test
  def testExternalizable() {
    val evStream = loadStreamFromResource("/org/freetle/input.xml")
    val baout = new ByteArrayOutputStream()
    val dataOut = new ObjectOutputStream(baout)
    dehydrate(evStream, dataOut)
    dataOut.writeObject(null)
    dataOut.flush
    assertTrue(baout.size > 0)
    val bain = new ByteArrayInputStream(baout.toByteArray)
    val dataIn = new ObjectInputStream(bain)
    val result = rehydrate(dataIn)
    assertEquals(evStream.size, result.size)
    assertEquals(evStream, result)
    
  }
}