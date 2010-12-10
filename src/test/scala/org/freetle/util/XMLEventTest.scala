package org.freetle.util

import org.junit._
import Assert._
import org.freetle.{TestXMLHelperMethods, CPSXMLModel, TestHelperMethods}

case class XMLEventTestContext
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
}