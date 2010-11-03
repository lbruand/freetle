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
package org.freetle.meta


import org.freetle.Transform

/**
 * a Meta processor is used to modify programmatically a preexisting transformation.
 */
@serializable @SerialVersionUID(599494944949L + 10002L)
trait Meta[Context] extends Transform[Context] {
  abstract class MetaProcessor extends (CFilterBase => CFilterBase)

  abstract class RecursiveMetaProcessor extends MetaProcessor {

    def map(in: BaseTransform) : CFilterBase

    def apply(in: CFilterBase) : CFilterBase = {

      in match {        
        case unary @ UnaryOperator(underlying) => unary.clone(this(underlying))
        case binary @ BinaryOperator(left, right) => binary.clone(this(left), this(right))
        case transfo : BaseTransform => map(transfo)
      }
    }
  }

  class SpaceSkipingMetaProcessor extends RecursiveMetaProcessor {
    def map(in: BaseTransform) = {
      if (!in.isInstanceOf[DropFilter]) {
        val takeSpc = new WhileNoResultOperator(new TakeSpace())
        new SequenceOperator(takeSpc, new SequenceOperator(in, takeSpc) )
      } else {
        in
      }
    }
  }
}