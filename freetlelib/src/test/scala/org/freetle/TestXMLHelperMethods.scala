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

import util._
import java.io.{StringWriter, InputStream}

/**
 * Various methods to load data from files.
 */
trait TestXMLHelperMethods[Context] extends CPSXMLModel[Context] with TestHelperMethods[XMLEvent, Context]{
  val PREFIX : String = "p"
  val NAMESPACE : String = "http://freetle.sf.net/"

  def mloadStreamFromResource(resourceName: String, context : Option[Context]): CPSStream = {
    val src: InputStream = this.getClass.getResourceAsStream(resourceName)
    XMLResultStreamUtils.loadXMLResultStream(src)
  }
  def sloadStreamFromResource(resourceName: String) = mloadStreamFromResource(resourceName, null)
  def loadStreamFromResource = new Memoize1(sloadStreamFromResource)
  // Utility methods used to test XMLResultStream

  /**
   * Serialize ( not very efficient ).
   */
  def serialize(x : CPSStream) : String = {
    var sb = new StringWriter()
    XMLResultStreamUtils.serializeXMLResultStream(x, sb)
    sb.toString
  }

   /**
   * Serialize ( not very efficient ).
   */
  def serializeWithResult(x : CPSStream) : String = {
    val charStream : Stream[Char] = (x map
            (y => Stream.cons(if (y._2) 'R' else 'T', y._1.getOrElse(new EvComment("EvEmptyPositive")).toStream))).flatten
    charStream.mkString
  }
  final def createStartElem(s : String) = new EvElemStart(new QName(NAMESPACE, s, PREFIX), null)
  final def createEndElem(s : String) = new EvElemEnd(new QName(NAMESPACE, s, PREFIX))
  final val filterIdentity = new CFilterIdentity()
}