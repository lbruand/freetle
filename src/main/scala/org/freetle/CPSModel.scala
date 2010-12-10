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
 // TODO : Explore the possibility of serializing the transforms with call-by-name member (left, right, underlying)
class CPSModel[Element, Context] {
  type CPSElementOrPositive = Option[Element]
  type CPSTupleElement = Tuple2[CPSElementOrPositive, Boolean]
  type CPSStream = Stream[CPSTupleElement]

  type CFilter = (CPSStream, Context) => CPSStream
  


  /**
   * An identity CFilter.
   */
  class CFilterIdentity extends CFilter {
    def apply(s : CPSStream, c : Context) : CPSStream = s
  }



  type InstantiateTransform = (()=>TransformBase)
  type InstantiateUnaryOperator = ((=>ChainedTransformRoot) => ChainedTransformRoot)
  type InstantiateBinaryOperator = ((=>ChainedTransformRoot, =>ChainedTransformRoot) => ChainedTransformRoot)

  /**
   * A metaProcessor to metatransform the transformations themselves. 
   */
  trait MetaProcessor {
    def processTransform(th :TransformBase, instantiate : InstantiateTransform) : ChainedTransformRoot
    def processUnaryOperator(th : UnaryOperator, instantiate : InstantiateUnaryOperator, underlying : =>ChainedTransformRoot) : ChainedTransformRoot
    def processBinaryOperator(th : BinaryOperator, instantiate : InstantiateBinaryOperator, left : =>ChainedTransformRoot, right : =>ChainedTransformRoot) : ChainedTransformRoot
  }
  /**
   *       t
   */
  trait MetaProcessable {
    def metaProcess(metaProcessor : MetaProcessor) : ChainedTransformRoot
  }

  /**
   * A type to be used only as a trait for the ChainedTransformRoot
   */
  type ChainedTransform = (=>CFilter, =>CFilter) => CFilter
  
  /**
   * Abstract class for all transformations.
   */
  abstract sealed class ChainedTransformRoot extends ChainedTransform with MetaProcessable {
    final def ~(other : => ChainedTransformRoot) : ChainedTransformRoot = new SequenceOperator(this, other)
    final def ->(other : => ChainedTransformRoot) : ChainedTransformRoot = new ComposeOperator(this, other)
    final def |(other : => ChainedTransformRoot) : ChainedTransformRoot = new ChoiceOperator(this, other)
    final def * : ChainedTransformRoot = new ZeroOrMoreOperator(this)
    final def + : ChainedTransformRoot = new OneOrMoreOperator(this)
    final def ? : ChainedTransformRoot = new ZeroOrOneOperator(this)
  }
  
  /**
   * HelperMethods on CPSStream.
   */
  object CPSStreamHelperMethods {
    final def removeWhileEmptyPositive(s : CPSStream) : CPSStream = s.dropWhile( x =>  x.equals( (None, true) ))
    
    final def removeAllEmptyPositive(s : CPSStream) : CPSStream = s.filter( x =>  !(x.equals( (None, true) )))

    final def isPositive(s : CPSStream) : Boolean = {
      if (s.isEmpty)
        false
      else
        s.head._2
    }

    final def isEmptyPositive(s : CPSStream) : Boolean = {
      isPositive(s) && removeWhileEmptyPositive(s).isEmpty
    }

    final def appendPositiveStream(s : CPSStream) : CPSStream = if (!isPositive(s))
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
        if (CPSStreamHelperMethods.isPositive(rs))
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
    final def metaProcess(metaProcessor : MetaProcessor) =
              metaProcessor.processBinaryOperator(this, new SequenceOperator(_, _), left, right)


    def apply(success : =>CFilter, failure : =>CFilter) : CFilter = {
      left(SequenceOperator.innerSequenceOperator(right(success, failure)), failure)
    }
  }
  object SequenceOperator {
    final private def innerSequenceOperator(input : =>CFilter)(s : CPSStream, c : Context) : CPSStream = {
      val (hd, tl) = s.span(_._2)
      val removedHd = CPSStreamHelperMethods.removeWhileEmptyPositive(hd)
      removedHd.append({input(tl, c)})
    }
  }
  
  class CFilterIdentityWithContext extends CFilter {
    var context : Option[Context] = None
    def isApplied : Boolean = !(None.equals(context))
    def apply(s : CPSStream, c : Context) : CPSStream = {
      context = Some(c)
      s
    }
  }
  /**
   * Composition Operator.
   */
  class ComposeOperator(left : =>ChainedTransformRoot, right : =>ChainedTransformRoot) extends BinaryOperator(left, right) {
    final def metaProcess(metaProcessor : MetaProcessor) =
              metaProcessor.processBinaryOperator(this, new ComposeOperator(_, _), left, right)
    
    def apply(success : =>CFilter, failure : =>CFilter) : CFilter = {
      (tl : CPSStream, c : Context) => {
        val identitySuccess = new CFilterIdentityWithContext()
        val identityFailure = new CFilterIdentityWithContext()
        val result = left(identitySuccess, identityFailure)(tl, c)
        result.force // This is needed because identitySuccess is only called at the end of the result Stream,
                     // as a side effect.
                     // But it is suboptimal in term of memory usage.
        if (identitySuccess.isApplied) {
          right(success, failure)(result, identitySuccess.context.get)
        } else {
          failure(tl, c)
        }
      }

    }
      //left(right(success, failure), failure)

  }

  /**
   * Choice Operator
   */
  class ChoiceOperator(left : =>ChainedTransformRoot, right : =>ChainedTransformRoot) extends BinaryOperator(left, right) {
    final def metaProcess(metaProcessor : MetaProcessor) =
              metaProcessor.processBinaryOperator(this, new ChoiceOperator(_, _), left, right)
    
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
    final def metaProcess(metaProcessor : MetaProcessor) =
              metaProcessor.processUnaryOperator(this, new OneOrMoreOperator(_), underlying)
    
    def apply(success : =>CFilter, failure : =>CFilter) : CFilter = {
      new SequenceOperator(underlying, new ZeroOrMoreOperator(underlying))(success, failure)
    }
  }
  /**
   * Cardinality operator 0..1
   */
  class ZeroOrOneOperator(underlying : =>ChainedTransformRoot) extends CardinalityOperator(underlying) {
    final def metaProcess(metaProcessor : MetaProcessor) =
              metaProcessor.processUnaryOperator(this, new ZeroOrOneOperator(_), underlying)
    
    def apply(success : =>CFilter, failure : =>CFilter) : CFilter = {
      underlying(success, CPSStreamHelperMethods.appendPositive(success))
    }
  }

  /**
   * Cardinality operator 0..* 
   */
  class ZeroOrMoreOperator(underlying : =>ChainedTransformRoot) extends CardinalityOperator(underlying) {
    final def metaProcess(metaProcessor : MetaProcessor) =
              metaProcessor.processUnaryOperator(this, new ZeroOrMoreOperator(_), underlying)
    
    def apply(success : =>CFilter, failure : =>CFilter) : CFilter = {
      new SequenceOperator(underlying, this)(success, CPSStreamHelperMethods.appendPositive(success))
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
   */                                          CPSStreamHelperMethods
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
    final def metaProcess(metaProcessor : MetaProcessor) =
              metaProcessor.processTransform(this, () => { new ElementMatcherTaker(matcher) })
    
    def partialapply(s : CPSStream) : CPSStream = {
      if (s.isEmpty)
        s
      else {
        val sr = CPSStreamHelperMethods.removeWhileEmptyPositive(s)
        if (matcher(sr.head._1.get))
          Stream.cons( (sr.head._1, true), sr.tail)
        else
          s
        }
    }
  }

  /**
   * A context-free transform, that drops all previous results.
   * It adds a EmptyPositive result if there was something to drop.
   */
  class DropFilter extends ContextFreeTransform {
    final def metaProcess(metaProcessor : MetaProcessor) =
              metaProcessor.processTransform(this, () => { this })
    
    def partialapply(s : CPSStream) : CPSStream = {
      if (CPSStreamHelperMethods.isPositive(s))
        CPSStreamHelperMethods.appendPositiveStream(s.dropWhile(_._2))
      else
        s
    }
  }

  /**
   * This is a template transform to select in the stream using an accumulator.
   */
  abstract class StatefulSelector[State] extends ContextFreeTransform {
    def conditionToStop(state : State) : Boolean
    def accumulate(state : State, element : CPSElementOrPositive) : State
    def initialState : State
    final private def recurse(in : CPSStream, currentState : State) : CPSStream = {
      if (in.isEmpty)
        Stream.empty
      else
      if (conditionToStop(currentState))
        in
      else
        Stream.cons( (in.head._1, true),
                    recurse(in.tail, accumulate(currentState, in.head._1)))
    }
    
    override def partialapply(in : CPSStream) : CPSStream = recurse(in, initialState)
  }
}
