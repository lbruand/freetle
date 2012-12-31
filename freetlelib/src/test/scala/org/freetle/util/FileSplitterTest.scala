 /*
  * Copyright 2010-2013 Lucas Bruand
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
import java.io._
import org.freetle.{FileSplitter, CPSXMLModel}

case class FileSplitterContext()
@Test
class FileSplitterTest extends CPSXMLModel[FileSplitterContext] with FileSplitter[FileSplitterContext]{



  @Test
  def test() {
    val str = """<Document>
    <File><doc1>euhgzuie zeufhenuzehf fzehfeuiezh1</doc1></File>
    <File><doc2>euhgzuie zeufhenuzehf fzehfeuiezh2</doc2></File>
    <File><doc3>euhgzuie zeufhenuzehf fzehfeuiezh3</doc3></File>
    </Document>
    """
    var inStream = XMLResultStreamUtils.loadXMLResultStream(str)
    inStream = inStream.tail
    serializeXMLResultStream(inStream, (occurrence, inputWriter) => {

      if (inputWriter != null) {
        val res = inputWriter.toString
        assertTrue("occurrence nb "+occurrence + " val=["+ res+"]", res.endsWith(""+occurrence+">"))
      }
      new StringWriter()
    }, context = null)

  }

}