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

import annotation.tailrec
import java.io.Writer
import util.{EvComment, XMLEvent}
import javax.xml.stream.{XMLStreamWriter, XMLOutputFactory}
import com.ctc.wstx.stax.WstxOutputFactory

/**
 * This trait can be used to cut a single stream into multiple files.
 * It matches repeatedly `fileMatcher` and each chunk thus matched can be
 * written into an writer created using `writerConstructor`.
 */

trait FileSplitter[Context] extends CPSXMLModel[Context] {
  val fileMatcher : ChainedTransformRoot = (((takeSpace*)  ~ <("File")) -> drop) ~  new DeepFilterUntil() ~
                                            (</("File") -> drop )
  /**
   * Serialise a XMLResultStream into a XML form.
   */
  final def serializeXMLResultStream(evStream : =>CPSStream,
                               writerConstructor : (Int, Writer) => Writer,
                               occurrence : Int = 0,
                               writerInput : Writer = null,
                               context : Context) {

    // TODO in the event of an exception the current writer is not closed.
    val trans = fileMatcher(new CFilterIdentity(), new CFilterIdentity())
    var that = evStream
    var myOcc = occurrence
    var carryOnWhileLoop = true
    var writer : Writer = writerInput

    while (carryOnWhileLoop) {
      that = trans(CPSStreamHelperMethods.turnToTail(that), context)

      var read: Boolean = false
      try {
        writer = writerConstructor(myOcc, writer)
        val outputFactory : WstxOutputFactory = new WstxOutputFactory()
        outputFactory.configureForSpeed()
        val xmlStreamWriter : XMLStreamWriter = outputFactory.createXMLStreamWriter(writer)
        while (!that.isEmpty && that.head._2) {

          that.head._1 match {
            case Some(x: XMLEvent) => {
              read = true
              x.appendTo(xmlStreamWriter)
            }
            case None => ()
          }
          that = that.tail
        }
        if (read) {
          xmlStreamWriter.close()
        }
        writer.flush()
      }
      catch {
        case e => {
          if (writer != null) {
            writer.close()
          }
          throw e
        }
      }
      carryOnWhileLoop = (!that.isEmpty && read)
      if (carryOnWhileLoop) { // Go ahead
        myOcc += 1
      }
    }



  }
}