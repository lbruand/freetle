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

/**
 * This is an abstract streaming Continuation Passing Transformation model.
 * It is capable of working over any type of alphabet (not only XML).
 * It is internally using continuations This was used to solve the stackoverflow problems
 * met in previous backtracking models. 
 */

class CPSModel[Element, Context] {
  type CPSElementOrPositive = Option[Element]
  type CPSTupleElement = Tuple2[CPSElementOrPositive, Boolean]
  type CPSStream = Stream[CPSTupleElement]

  type CFilter = (CPSStream, Context) => CPSStream //Function2[CPSStream, Context, CPSStream]

  type ChainedTransform = (=>CFilter, =>CFilter) => CFilter
  class CFilterIdentity extends CFilter {
    def apply(s : CPSStream, c : Context) : CPSStream = s
  }

  abstract class TransformBase extends ChainedTransform {
    def apply(s : CPSStream, c : Context) : (CPSStream, Context)

    def isPositive(s : CPSStream) : Boolean = {
      if (s.isEmpty)
        false
      else
        s.head._2
    }

    def apply(success : =>CFilter, failure : =>CFilter) : CFilter = {
      (s : CPSStream, c : Context) => {
        val (rs, rc) = apply(s, c)
        if (isPositive(rs))
          success(rs, rc)
        else
          failure(s, c)
      }
    }
  }
  class SequenceOperator(left : ChainedTransform, right : ChainedTransform) extends ChainedTransform {
    def apply(success : =>CFilter, failure : =>CFilter) : CFilter = {
      left( (s : CPSStream, c : Context) => {
              val (hd, tl) = s.span(_._2)
              hd.append( { right(success, failure)(tl, c) })
            }
            , failure)
    }
  }

  class RepeatingOperator(underlying : ChainedTransform) extends ChainedTransform {
    def apply(success : =>CFilter, failure : =>CFilter) : CFilter = {
      new SequenceOperator(underlying, this)(success, failure)
    }
  }

  abstract class ContextFreeTransform extends TransformBase {
    def projapply(s : CPSStream) : CPSStream
    def apply(s : CPSStream, c : Context) : (CPSStream, Context) = (projapply(s), c)
  }

  abstract class ContextWritingTransform extends TransformBase {
    def projapply(s : CPSStream, c : Context) : Context
    def apply(s : CPSStream, c : Context) : (CPSStream, Context) = (s, projapply(s, c))
  }

  abstract class ContextReadingTransform extends TransformBase {
    def projapply(s : CPSStream, c : Context) : CPSStream
    def apply(s : CPSStream, c : Context) : (CPSStream, Context) = (projapply(s, c), c)
  }


  type CPSElemMatcher = Element => Boolean

  class Taker(matcher : CPSElemMatcher)  extends ContextFreeTransform {
    def projapply(s : CPSStream) : CPSStream = {
      if (s.isEmpty)
        s
      else
        if (matcher(s.head._1.get))
          Stream.cons( (s.head._1, true), s.tail)
        else
          s
    }
  }
}
