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
 // TODO : Explore if it is not possible to get rid of isPositive and the emptyPositive completely using CPS.
class CPSModel[Element, Context] {
  type CPSElementOrPositive = Option[Element]
  type CPSTupleElement = Tuple2[CPSElementOrPositive, Boolean]
  type CPSStream = Stream[CPSTupleElement]

  type CFilter = (CPSStream, Context) => CPSStream
  
  type ChainedTransform = (=>CFilter, =>CFilter) => CFilter

  /**
   * An identity CFilter.
   */
  class CFilterIdentity extends CFilter {
    def apply(s : CPSStream, c : Context) : CPSStream = s
  }
  abstract class ChainedTransformRoot extends ChainedTransform with CPSStreamHelperMethods {
    final def ~(other : => ChainedTransformRoot) : ChainedTransformRoot = new SequenceOperator(this, other)
    final def ->(other : => ChainedTransformRoot) : ChainedTransformRoot = new ComposeOperator(other, this)
    final def * : ChainedTransformRoot = new ZeroOrMoreOperator(this)
    final def + : ChainedTransformRoot = new OneOrMoreOperator(this)
    final def ? : ChainedTransformRoot = new ZeroOrOneOperator(this)
  }
  /**
   * HelperMethods on CPSStream.
   */
  trait CPSStreamHelperMethods {
    final def removeWhileEmptyPositive(s : CPSStream) : CPSStream = s.dropWhile( x =>  x.equals( (None, true) ))

    final def isPositive(s : CPSStream) : Boolean = {
      if (s.isEmpty)
        false
      else
        s.head._2
    }

    final private def appendPositiveStream(s : CPSStream) : CPSStream = if (!isPositive(s))
                                                                          Stream.cons((None, true), s)
                                                                        else
                                                                          s
    
    final private def innerAppendPositive(input : =>CFilter)(str : CPSStream, c : Context) : CPSStream =
                                                                        input(appendPositiveStream(str), c)

    final def appendPositive(input : =>CFilter) : CFilter = innerAppendPositive(input)
    
  }
  /**
   *  Base class for all transforms.
   */
  abstract class TransformBase extends ChainedTransformRoot  {
    def apply(s : CPSStream, c : Context) : (CPSStream, Context)

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

  /**
   * Base class for Operators. (Transforms that combine two transforms)
   */
  abstract class Operator extends ChainedTransformRoot

  /**
   * A Base class for Unary operators (which modify just one transform)
   */
  abstract class UnaryOperator(underlying : =>ChainedTransformRoot) extends Operator

  /**
   * A base class for Binary operators (which combine two different transforms, named left and right)
   */
  abstract class BinaryOperator(left : =>ChainedTransformRoot, right : =>ChainedTransformRoot) extends Operator

  /**
   * We execute in sequence left and then right if left has returned a result. 
   */
  class SequenceOperator(left : =>ChainedTransformRoot, right : =>ChainedTransformRoot) extends BinaryOperator(left, right) {
    final private def innerSequenceOperator(input : =>CFilter)(s : CPSStream, c : Context) : CPSStream = {
        val (hd, tl) = s.span(_._2)
        hd.append( { input(tl, c) })
    }
    def apply(success : =>CFilter, failure : =>CFilter) : CFilter = {
      left(innerSequenceOperator(right(success, failure)), failure)
    }
  }

  /**
   * Composition Operator.
   */
  class ComposeOperator(left : =>ChainedTransformRoot, right : =>ChainedTransformRoot) extends BinaryOperator(left, right) {
    def apply(success : =>CFilter, failure : =>CFilter) : CFilter = left(right(success, failure), failure)
  }

  /**
   * Choice Operator
   */
  class ChoiceOperator(left : =>ChainedTransformRoot, right : =>ChainedTransformRoot) extends BinaryOperator(left, right) {
    def apply(success : =>CFilter, failure : =>CFilter) : CFilter = left(success, right(success, failure))
  }

  /**
   *   Base class for cardinality operators.
   */
  abstract class CardinalityOperator(underlying : =>ChainedTransformRoot) extends UnaryOperator(underlying)
  
  /**
   * Cardinality operator 1..*
   */
  class OneOrMoreOperator(underlying : =>ChainedTransformRoot) extends CardinalityOperator(underlying) {
    def apply(success : =>CFilter, failure : =>CFilter) : CFilter = {
      new SequenceOperator(underlying, new ZeroOrMoreOperator(underlying))(success, failure)
    }
  }
  /**
   * Cardinality operator 0..1
   */
  class ZeroOrOneOperator(underlying : =>ChainedTransformRoot) extends CardinalityOperator(underlying) {

    def apply(success : =>CFilter, failure : =>CFilter) : CFilter = {
      underlying(success, appendPositive(success)) // TODO Not enough I think... need to add a EmptyPositive to be sure.
    }
  }

  /**
   * Cardinality operator 0..* 
   */
  class ZeroOrMoreOperator(underlying : =>ChainedTransformRoot) extends CardinalityOperator(underlying) {
    def apply(success : =>CFilter, failure : =>CFilter) : CFilter = {
      new SequenceOperator(underlying, this)(success, appendPositive(success)) // TODO Not enough I think... need to add a EmptyPositive to be sure.
    }
  }

  /**
   * A transform that's context-free
   * (Not using the context, either as input or ouput)
   */
  abstract class ContextFreeTransform extends TransformBase {
    def partialapply(s : CPSStream) : CPSStream
    final def apply(s : CPSStream, c : Context) : (CPSStream, Context) = (partialapply(s), c)
  }

  /**
   * A transform that's modifying the context but not the stream.
   */
  abstract class ContextWritingTransform extends TransformBase {
    def partialapply(s : CPSStream, c : Context) : Context
    final def apply(s : CPSStream, c : Context) : (CPSStream, Context) = (s, partialapply(s, c))
  }

  /**
   * A transform that's using the context to modify the stream.
   */
  abstract class ContextReadingTransform extends TransformBase {
    def partialapply(s : CPSStream, c : Context) : CPSStream
    final def apply(s : CPSStream, c : Context) : (CPSStream, Context) = (partialapply(s, c), c)
  }

  /**
   * A function that matches elements.
   */
  type CPSElemMatcher = Element => Boolean

  /**
   * A Context-free transform that matches elements.
   */
  class ElementMatcherTaker(matcher : CPSElemMatcher)  extends ContextFreeTransform {
    
    def partialapply(s : CPSStream) : CPSStream = {
      if (s.isEmpty)
        s
      else {
        val sr = removeWhileEmptyPositive(s)
        if (matcher(sr.head._1.get))
          Stream.cons( (sr.head._1, true), sr.tail)
        else
          s
        }
    }
  }
}
