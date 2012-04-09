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


class CPSModelSerializable[Element <: java.io.Serializable, Context] extends CPSModel[Element, Context] {
/**
   * The Sort Operator is used to :
   * 1. Tokenize into items using the tokenizer.
   * 2. Sort according to a key extracted by the keyExtractor.
   * Everything is done in-memory.
   */
  class SortOperator(tokenizer : =>ChainedTransformRoot, keyExtractor : =>ChainedTransformRoot) extends BinaryOperator(tokenizer, keyExtractor) {
    def metaProcess(metaProcessor : MetaProcessor) =
              metaProcessor.processBinaryOperator(this, new SortOperator(_, _), tokenizer, keyExtractor)
    lazy val tokenizerRealized : ChainedTransformRoot = tokenizer
    lazy val keyExtractorRealized : ChainedTransformRoot = keyExtractor

    def apply(success : =>CFilter, failure : =>CFilter) : CFilter = sortInternal(success, failure)

    private def sortInternal(success : =>CFilter, failure : =>CFilter)(inputStream : CPSStream, context : Context) : CPSStream= {
      var listBuffer = new ListBuffer[CPSStream]()
      var currentStream = inputStream

      // Split the stream into the listBuffer.
      var hd : CPSStream = null
      var tl : CPSStream = null
      do  {
        val result = tokenizerRealized(new CFilterIdentity(), new CFilterIdentity())(currentStream, context)
        val resTuple = result.span(p => p._2)
        hd = resTuple._1
        tl = resTuple._2
        currentStream = tl
        listBuffer.append(hd)
      } while (!hd.isEmpty)
      currentStream = null

      // Insert all the listBuffer into a TreeMap.
      val treeMap = TreeMap.empty[String, CPSStream] ++ ((listBuffer map (generator(keyExtractorRealized)(_, context))).toIterator)

      // get all values then flatten them into an unique stream.
      val resultStream = treeMap.values.flatten.toStream
      success(resultStream, context)
    }

    private final def generator(keyExtractor : ChainedTransformRoot)(x : CPSStream, context : Context) : (String, CPSStream) = ((keyExtractor(new CFilterIdentity(), new CFilterIdentity())(x, context).mkString) -> x)
  }
}