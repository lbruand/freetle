 /*
  * Copyright 2010-2011 Lucas Bruand
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
 * Defines types for the CPSModel.
 */
trait CPSModelTypeDefinition[@specialized Element, @specialized Context] {
  type CPSElementOrPositive = Option[Element]
  type CPSTupleElement = (CPSElementOrPositive, Boolean)

  /**
   * A property not enforceable with the type system :
   *  * At a certain rank the stream, all elements are tail (i.e. _2 == false)
   *  * Before that certain rank, all elements are results (there can possibly be no result at all)
   */
  type CPSStream = Stream[CPSTupleElement]

  type CFilter = (CPSStream, Context) => CPSStream

  /**
   * A type to be used only as a trait for the ChainedTransformRoot
   */
  type ChainedTransform = (=>CFilter, =>CFilter) => CFilter


    /**
   * HelperMethods on CPSStream.
   */
  trait CPSStreamHelperMethodsTrait {
    @inline final def isEmptyPositive(x : CPSTupleElement) : Boolean = x.equals( (None, true) )
    @inline final def isNotEmptyPositive(x : CPSTupleElement) : Boolean = !(isEmptyPositive(x))
    @inline final def removeWhileEmptyPositive(s : CPSStream) : CPSStream = {
      var cs : CPSStream = s
      while ( !cs.isEmpty && CPSStreamHelperMethods.constantEmptyPositive.equals(cs.head) ) {
        cs = cs.tail
      }
      cs
    }

    def removeAllEmptyPositive(s : CPSStream) : CPSStream = s.filter( isNotEmptyPositive )

    @inline final def isPositive(s : CPSStream) : Boolean = {
      if (s.isEmpty)
        false
      else
        s.head._2
    }

    def isOnlyEmptyPositiveStream(s : CPSStream) : Boolean = {
      isPositive(s) && removeWhileEmptyPositive(s).isEmpty
    }

    @inline final def appendPositiveStream(s : CPSStream) : CPSStream = if (!isPositive(s))
                                                                          Stream.cons(CPSStreamHelperMethods.constantEmptyPositive, s)
                                                                        else
                                                                          s

    private def innerAppendPositive(input : =>CFilter)(str : CPSStream, c : Context) : CPSStream =
                                                                        input(appendPositiveStream(str), c)

    @inline final def appendPositive(input : =>CFilter) : CFilter = innerAppendPositive(input)
  }
}

/**
 * This is an abstract streaming Continuation Passing Transformation model.
 * It is capable of working over any type of alphabet (not only XML).
 * It is internally using continuations This was used to solve the stackoverflow problems
 * met in previous backtracking models.
 */
class CPSModel[@specialized Element, @specialized Context] extends CPSModelTypeDefinition[Element, Context] {
  /**
   *  An identity CFilter.
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
   * Something that is usable by a MetaProcessor.
   */
  trait MetaProcessable {
    def metaProcess(metaProcessor : MetaProcessor) : ChainedTransformRoot
  }


  /**
   * Abstract class for all transformations.
   * It defines shortcuts for operators.
   */
  abstract sealed class ChainedTransformRoot extends ChainedTransform with MetaProcessable with CPSStreamHelperMethodsTrait {
    final def ~(other : => ChainedTransformRoot) : ChainedTransformRoot = new SequenceOperator(this, other)
    final def ->(other : => ChainedTransformRoot) : ChainedTransformRoot = new ComposeOperator(this, other)
    final def |(other : => ChainedTransformRoot) : ChainedTransformRoot = new ChoiceOperator(this, other)
    final def * : ChainedTransformRoot = new ZeroOrMoreOperator(this)
    final def + : ChainedTransformRoot = new OneOrMoreOperator(this)
    final def ? : ChainedTransformRoot = new ZeroOrOneOperator(this)
  }
  object CPSStreamHelperMethods extends CPSStreamHelperMethodsTrait {
    @inline val constantEmptyPositive : CPSTupleElement = (None, true)
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
  final class SequenceOperator(left : =>ChainedTransformRoot, right : =>ChainedTransformRoot) extends BinaryOperator(left, right) {
    def metaProcess(metaProcessor : MetaProcessor) =
              metaProcessor.processBinaryOperator(this, new SequenceOperator(_, _), left, right)
    lazy val leftRealized : ChainedTransformRoot = left
    lazy val rightRealized : ChainedTransformRoot = right

    def apply(success : =>CFilter, failure : =>CFilter) : CFilter = {
      leftRealized(new InnerSequenceOperator(rightRealized(success, failure)), failure)
    }
    
    final class InnerSequenceOperator(input : =>CFilter) extends CFilter {
      def apply(s : CPSStream, c : Context) : CPSStream = {
        if (s.isEmpty) {
          input(s, c)
        } else {
          if (s.head._2) { // It is Result.
            s.head._1 match {
              case None => this(s.tail, c) // Empty positive to trim.
              case Some(_) => Stream.cons(s.head, this(s.tail, c)) // Non empty positive to skip.
            }
          } else {
            input(s, c)
          }
        }
      }
    }
  }

  
  final class CFilterIdentityWithContext extends CFilter {
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

  final class ComposeOperator(left : =>ChainedTransformRoot, right : =>ChainedTransformRoot) extends BinaryOperator(left, right) {
    def metaProcess(metaProcessor : MetaProcessor) =
              metaProcessor.processBinaryOperator(this, new ComposeOperator(_, _), left, right)
    lazy val leftRealized : ChainedTransformRoot = left
    lazy val rightRealized : ChainedTransformRoot = right
    @inline private def forceStream(result: CPSStream, identitySuccess: CFilterIdentityWithContext, identityFailure: CFilterIdentityWithContext): Unit = {
      var these = result
      while (!identitySuccess.isApplied && !identityFailure.isApplied && !these.tail.isEmpty) these = these.tail
    }
    def apply(success : =>CFilter, failure : =>CFilter) : CFilter = {
      (tl : CPSStream, c : Context) => {
        val identitySuccess = new CFilterIdentityWithContext()
        val identityFailure = new CFilterIdentityWithContext()
        val result = leftRealized(identitySuccess, identityFailure)(tl, c)
        forceStream(result, identitySuccess, identityFailure)
                     // This is needed because identitySuccess is only called at the end of the result Stream,
                     // as a side effect.
                     // But it is suboptimal in term of memory usage.
                     // We need identitySuccess to be instantiated so that we can pass further on the context.
        if (identitySuccess.isApplied) {
          rightRealized(success, failure)(result, identitySuccess.context.get)
        } else {
          failure(tl, c)
        }
      }

    }

  }

  /**
   * Choice Operator
   */
  final class ChoiceOperator(left : =>ChainedTransformRoot, right : =>ChainedTransformRoot) extends BinaryOperator(left, right) {
    def metaProcess(metaProcessor : MetaProcessor) =
              metaProcessor.processBinaryOperator(this, new ChoiceOperator(_, _), left, right)
    lazy val leftRealized : ChainedTransformRoot = left
    lazy val rightRealized : ChainedTransformRoot = right
    def apply(success : =>CFilter, failure : =>CFilter) : CFilter = leftRealized(success, rightRealized(success, failure))
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
    lazy val underlyingRealized : ChainedTransformRoot = underlying
    lazy val seqOneOrMoreOperator : ChainedTransformRoot = new SequenceOperator(underlyingRealized, new ZeroOrMoreOperator(underlying))
    def apply(success : =>CFilter, failure : =>CFilter) : CFilter = {
      seqOneOrMoreOperator(success, failure)
    }
  }
  /**
   * Cardinality operator 0..1
   */
  final class ZeroOrOneOperator(underlying : =>ChainedTransformRoot) extends CardinalityOperator(underlying) {
    def metaProcess(metaProcessor : MetaProcessor) =
              metaProcessor.processUnaryOperator(this, new ZeroOrOneOperator(_), underlying)
    lazy val underlyingRealized : ChainedTransformRoot = underlying
    def apply(success : =>CFilter, failure : =>CFilter) : CFilter = {
      underlyingRealized(success, appendPositive(success))
    }
  }

  /**
   * Cardinality operator 0..* 
   */
  final class ZeroOrMoreOperator(underlying : =>ChainedTransformRoot) extends CardinalityOperator(underlying) {
    def metaProcess(metaProcessor : MetaProcessor) =
              metaProcessor.processUnaryOperator(this, new ZeroOrMoreOperator(_), underlying)
    lazy val underlyingRealized : ChainedTransformRoot = underlying
    lazy val sequenceOperator = new SequenceOperator(underlyingRealized, this)
    def apply(success : =>CFilter, failure : =>CFilter) : CFilter = {
      sequenceOperator(success, appendPositive(success))
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
    def apply(s : CPSStream, c : Context) : (CPSStream, Context)
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
  final class ElementMatcherTaker(matcher : CPSElemMatcher)  extends ContextFreeTransform {
    def metaProcess(metaProcessor : MetaProcessor) =
              metaProcessor.processTransform(this, () => { new ElementMatcherTaker(matcher) })
    
    @inline def partialapply(s : CPSStream) : CPSStream = {
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

  /**
   * A context-free transform, that drops all previous results.
   * It adds a EmptyPositive result if there was something to drop.
   */
  object drop extends ContextFreeTransform {
    def metaProcess(metaProcessor : MetaProcessor) =
              metaProcessor.processTransform(this, () => { this })
    
    def partialapply(s : CPSStream) : CPSStream = {
      if (isPositive(s))
        appendPositiveStream(s.dropWhile(_._2))
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
