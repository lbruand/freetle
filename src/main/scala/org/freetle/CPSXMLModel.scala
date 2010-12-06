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
package org.freetle

import util.{EvElemEnd, EvElemStart, XMLEvent}

/**
 * This is a streaming Continuation Passing Transformation model.
 * It is capable of working over XML Events.
 */

class CPSXMLModel[Context] extends CPSModel[XMLEvent, Context] {
  class DeepFilter extends StatefulSelector[Int] {
    def metaProcess(metaProcessor: MetaProcessor) = metaProcessor.processTransform(() => { new DeepFilter() })
    def conditionToStop(depth: Int) = depth < 0

    def accumulate(depth: Int, element: CPSElementOrPositive) : Int = depth + (element match {
      case e: EvElemStart => +1
      case e: EvElemEnd => -1
      case _ => 0
    })
    
    def initialState = 1
  }
}