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
package org.freetle.util
import org.junit._
import Assert._
import org.freetle.{TestXMLHelperMethods, CPSXMLModel}
import java.io._
import annotation.tailrec

case class FileSplitterContext()
@Test
class FileSplitterTest extends CPSXMLModel[FileSplitterContext]{
  val fileMatcher : ChainedTransformRoot = ((takeSpace*) -> drop) ~ <("File") ~ new DeepFilter()
  /**
   * Serialise a XMLResultStream into a XML form.
   */
  @tailrec final def serializeXMLResultStream(evStream : =>CPSStream,
                               writerConstructor : (Int, Writer) => Writer,
                               occurrence : Int = 0,
                               writerInput : Writer = null) : Unit = {
    val trans = fileMatcher(new CFilterIdentity(), new CFilterIdentity())
    var that = trans(CPSStreamHelperMethods.turnToTail(evStream), null)
    val writer = writerConstructor(occurrence, writerInput)
    var read : Boolean = false
    while (!that.isEmpty && that.head._2) {
      read = true
      that.head._1 match {
              case Some(x : XMLEvent) => x.appendWriter(writer)
              case _ => (new EvComment("EmptyPositive")).appendWriter(writer)
              }
      that = that.tail
    }

    writer.flush()
    if (!that.isEmpty && read) {
      serializeXMLResultStream(that,
                               writerConstructor = writerConstructor,
                               occurrence = occurrence + 1,
                               writer)
    }
  }


  @Test
  def test() = {
    val str = """<Document>
    <File>euhgzuie zeufhenuzehf fzehfeuiezh1</File>
    <File>euhgzuie zeufhenuzehf fzehfeuiezh2</File>
    <File>euhgzuie zeufhenuzehf fzehfeuiezh3</File>
    </Document>
    """
    var inStream = XMLResultStreamUtils.loadXMLResultStream(str)
    inStream = inStream.tail
    serializeXMLResultStream(inStream, (occurrence, inputWriter) => {

      if (inputWriter != null) {
        val res = inputWriter.toString
        assertTrue("occurrence nb "+occurrence + " val=["+ res+"]", res.endsWith(""+occurrence+"</File>"))
      }
      new StringWriter()
    })

  }

}