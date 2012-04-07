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

import collection.mutable.ListBuffer
import collection.immutable.TreeMap

/**
 * The Sort trait is used to sort a stream.
 */

trait Sort[Element, Context] extends CPSModel[Element, Context]{
  private final def generator(keyExtractor : ChainedTransformRoot)(x : CPSStream, context : Context) : (String, CPSStream) = ((keyExtractor(new CFilterIdentity(), new CFilterIdentity())(x, context).mkString) -> x)
  /**
   *
   * @param itemSplitter used to split items before sorting them.
   * @param keyExtractor extract a key out of an item.
   * @param evStream
   * @param context
   * @return
   */
  final def sort(itemSplitter : ChainedTransformRoot, keyExtractor : ChainedTransformRoot)(evStream : => CPSStream, context : Context) : CPSStream = {
    var listBuffer = new ListBuffer[CPSStream]()
    var currentStream = evStream

    // Split the stream into the listBuffer.
    while (!currentStream.isEmpty) {
      val result = itemSplitter(new CFilterIdentity(), new CFilterIdentity())(currentStream, context)
      val (hd, tl) = result.span(p => p._2)
      currentStream = tl
      listBuffer.append(hd)
    }
    currentStream = null

    // Insert all the listBuffer into a TreeMap.
    val treeMap = TreeMap.empty[String, CPSStream] ++ ((listBuffer map (generator(keyExtractor)(_, context))).toIterator)

    // get all values then flatten them into an unique stream.
    treeMap.values.flatten.toStream
  }
}