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
 * Defines types for the CPSModel.
 */
trait CPSModelTypeDefinition[@specialized Element, @specialized Context] {
  /** A type that contains either Some(Element) or None  */
  type CPSElementOrPositive = Option[Element]
  /**
   * A tuple that contains either Some(Element) or None in _1 and a boolean in _2
   * which indicates whether this Element is a Result (value true) or a Tail
   * (value false).
   */
  type CPSTupleElement = (CPSElementOrPositive, Boolean)

  /**
   * A property not enforceable with the type system :
   *  * At a certain rank the stream, all elements are tail (i.e. _2 == false)
   *  * Before that certain rank, all elements are results (there can possibly be no result at all)
   */
  type CPSStream = Stream[CPSTupleElement]

  /**
   * CFilter type is the basic type of transformations in Freetle.
   */
  type CFilter = (CPSStream, Context) => CPSStream

  /**
   * A type to be used only as a trait for the ChainedTransformRoot
   *
   * the first parameter refers to the success continuation. (Called whenever the current trans. is a success)
   *
   * the second parameter refers to the failure continuation. (Called whenever the current trans. fails)
   */
  type ChainedTransform = (=>CFilter, =>CFilter) => CFilter

}

/**
 * The CPSModelHelperExtension layer regroups Helper functions that are not exposed uplayer.
 */
trait CPSModelHelperExtension[@specialized Element, @specialized Context] extends CPSModelTypeDefinition[Element, Context]{

  /**
   * The object CPSStreamHelperMethods is a singleton to store low-level helper functions.
   */
  object CPSStreamHelperMethods {
    @inline val constantEmptyPositive : CPSTupleElement = (None, true)
    @inline def isEmptyPositive(x : CPSTupleElement) : Boolean = x.equals( constantEmptyPositive )
    @inline def isNotEmpty(x : CPSTupleElement) : Boolean = (!x._1.isEmpty)
    @inline def isNotEmptyPositive(x : CPSTupleElement) : Boolean = !(isEmptyPositive(x))
    @inline def removeWhileEmptyPositive(s : CPSStream) : CPSStream = {
      var cs : CPSStream = s
      while ( !cs.isEmpty && CPSStreamHelperMethods.constantEmptyPositive.equals(cs.head) ) {
        cs = cs.tail
      }
      cs
    }

    /**
     * removes all the empty Positives in the stream.
     */
    def removeAllEmptyPositive(s : CPSStream) : CPSStream = {
      val (result, tail) = s.span(_._2)
      result.filter(isNotEmptyPositive).append(tail)
    }

    /**
     * Is the stream positive.
     * A stream is positive if:
     *  * it is not empty
     *  * and its head is positive.
     */
    @inline def isPositive(s : CPSStream) : Boolean = {
      if (s.isEmpty)
        false
      else
        s.head._2
    }

    /**
     * Is the stream only constituted by empty positives.
     */
    def isOnlyEmptyPositiveStream(s : CPSStream) : Boolean = {
      isPositive(s) && removeWhileEmptyPositive(s).isEmpty
    }

    /**
     * Make the stream positive if it is not already by appending an empty positive
     * ahead.
     */
    @inline def appendPositiveStream(s : CPSStream) : CPSStream = if (!isPositive(s))
                                                                          Stream.cons(CPSStreamHelperMethods.constantEmptyPositive, s)
                                                                        else
                                                                          s

    /**
     * Pass only a positive stream to the `input` continuation. (private impl.)
     */
    private def innerAppendPositive(input : =>CFilter)(str : CPSStream, c : Context) : CPSStream =
                                                                        input(appendPositiveStream(str), c)

    /**
     * Pass only a positive stream to the `input` continuation
     */
    @inline def appendPositive(input : =>CFilter) : CFilter = innerAppendPositive(input)

    /**
     * Turn any result into a negative (tail).
     */
    @inline def turnToTail(s : CPSStream) : CPSStream = s filter (isNotEmpty) map (x => (x._1, false))
  }
}

/**
 * This is an abstract streaming Continuation Passing Transformation model.
 * It is capable of working over any type of alphabet (not just XML).
 * It is internally using continuations This was used to solve the stackoverflow problems
 * met in previous backtracking models.
 *
 * == defining transformation expression ==
 *
 * CPSModel let you define transformation by writing expressions using :
 *  - A small number of unit transforms
 *  - Operators that let you combine sub-expressions.
 *
 *  example of an expression defined as:
 *
 *  {{{
 *  val t = 'a' ~ ('b'*) ~ ('a' | 'c')
 *  }}}
 *
 * <object type="image/svg+xml" data="expression.svg" width="348" height="330">
 *   <param name="src" value="expression.svg">
 *   alt : <a href="expression.svg">graph representing the above expression</a>
 * </object>
 *
 *  recognizes:
 *
 *  - abbbba
 *  - abbc
 *  - aa
 *  - ac
 *
 *  but NOT :
 *  - aabbc
 *  - a *
 *
 * == Running expressions ==
 *
 * CPSModel uses [[http://en.wikipedia.org/wiki/Continuation-passing_style Continuation Passing Style]] to run
 * the transformation in bounded memory.
 *
 * At runtime, the expression graph is converted lazily into a continuation graph. For example :
 * <object type="image/svg+xml" data="cps.svg" width="1691" height="446">
 *   <param name="src" value="cps.svg">
 *   alt : <a href="cps.svg">graph representing a continuation graph</a>
 * </object>
 *
 */
class CPSModel[@specialized Element, @specialized Context] extends CPSModelHelperExtension[Element, Context] {
  /**
   *  An identity CFilter.
   *  it does simply return the incoming stream as it is.
   */
  class CFilterIdentity extends CFilter {
    def apply(s : CPSStream, c : Context) : CPSStream = s
  }

  /** An exception raised by FailsCFilter */
  class ParsingFailure(msg :String) extends RuntimeException(msg)

  /**
   * Failure CFilter.
   * @throws ParsingFailure raised whenever parsing fails and this continuation is called.
   */
  class FailsCFilter extends CFilter {
    def apply(s : CPSStream, c : Context) : CPSStream = {
      throw new ParsingFailure("Parsing failed")
    }
  }


  /**
   * A Type that describes a constructor function for TransformBase and subtypes.
   * Used by the MetaProcessor.
   */
  type InstantiateTransform = (()=>TransformBase)
  /**
   * A Type that describes a constructor function for a UnaryOperator.
   */
  type InstantiateUnaryOperator = ((=>ChainedTransformRoot) => ChainedTransformRoot)
  /**
   * A Type that describes a constructor function for a BinaryOperator.
   */
  type InstantiateBinaryOperator = ((=>ChainedTransformRoot, =>ChainedTransformRoot) => ChainedTransformRoot)
  /**
   * A Type that describes a constructor function for a MultiOperator.
   */
  type InstantiateMultiOperator = (Map[String, ()=>ChainedTransformRoot] => ChainedTransformRoot)


  /**
   * A metaProcessor to metatransform the transformations themselves. 
   */
  trait MetaProcessor {
    def processTransform(th :TransformBase, instantiate : InstantiateTransform) : ChainedTransformRoot
    def processUnaryOperator(th : UnaryOperator, instantiate : InstantiateUnaryOperator, underlying : =>ChainedTransformRoot) : ChainedTransformRoot
    def processBinaryOperator(th : BinaryOperator, instantiate : InstantiateBinaryOperator, left : =>ChainedTransformRoot, right : =>ChainedTransformRoot) : ChainedTransformRoot
    def processMultiOperator(th : MultiOperator, instantiate : InstantiateMultiOperator, map : Map[String, () => ChainedTransformRoot]) :ChainedTransformRoot
  }
  /**
   * Something that is usable by a MetaProcessor.
   */
  trait MetaProcessable {
    def metaProcess(metaProcessor : MetaProcessor) : ChainedTransformRoot
  }


  /**
   * Abstract class for all transformations.
   * It defines many shortcuts for operators.
   */
  abstract sealed class ChainedTransformRoot extends ChainedTransform with MetaProcessable {
    /**
     * A shortcut to the sequence operator.
     * {{{
     * val t = a ~ b
     * }}}
     *
     * The t transformation recognizes the string "ab".
     */
    final def ~(other : => ChainedTransformRoot) : ChainedTransformRoot = new SequenceOperator(this, other)

    /**
     * A shortcut to the andThen operator. (reverse of the compose operator).
     */
    final def ->(other : => ChainedTransformRoot) : ChainedTransformRoot = new ComposeOperator(this, other)

    /**
     * A shortcut to ContextFree andThen operator (reverse of context free compose operator).
     */
    final def !->(other : => ChainedTransformRoot) : ChainedTransformRoot = new ContextFreeComposeOperator(this, other)

    /**
     * A shortcut to the choice operator.
     */
    final def |(other : => ChainedTransformRoot) : ChainedTransformRoot = new ChoiceOperator(this, other)

    /**
     * A shortcut to the zero or more cardinality operator.
     * @see http://en.wikipedia.org/wiki/Kleene_star
     */
    final def * : ChainedTransformRoot = new ZeroOrMoreOperator(this)

    /**
     * A shortcut to the at least one cardinality operator.
     * a+ is equivalent to aa*.
     */
    final def + : ChainedTransformRoot = new OneOrMoreOperator(this)

    /**
     * A shortcut to the optional (zero or one) cardinality operator.
     */
    final def ? : ChainedTransformRoot = new ZeroOrOneOperator(this)
  }


  /**
   *  Base class for all transforms.
   */
  abstract class TransformBase extends ChainedTransformRoot {
    def apply(s : CPSStream, c : Context) : (CPSStream, Context)

    private final def callSuccessOrFailure(success : =>CFilter, failure : =>CFilter)(s : CPSStream, c : Context) = {
      val (rs, rc) = apply(s, c)
      if (CPSStreamHelperMethods.isPositive(rs)) {
        success(rs, rc)
      } else {
        failure(s, c)
      }
    }

    def apply(success : =>CFilter, failure : =>CFilter) : CFilter =
          callSuccessOrFailure(success, failure)

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
   * A base class for Binary operators (which combine two different transforms, named left and right)
   */
  abstract class MultiOperator(map : Map[String, ()=>ChainedTransformRoot]) extends Operator

  /**
   * Executes in sequence the left operand and then the right operand if left operand has returned a result.
   */
  final class SequenceOperator(left : =>ChainedTransformRoot, right : =>ChainedTransformRoot) extends BinaryOperator(left, right) {
    def metaProcess(metaProcessor : MetaProcessor) =
              metaProcessor.processBinaryOperator(this, new SequenceOperator(_, _), left, right)
    lazy val leftRealized : ChainedTransformRoot = left
    lazy val rightRealized : ChainedTransformRoot = right

    def apply(success : =>CFilter, failure : =>CFilter) : CFilter = {
      leftRealized(new InnerSequenceOperator({
        rightRealized(success, failure)
      }), failure)
    }
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

  /**
   * An Identity CFilter that keeps that context in reference.
   */
  final class CFilterIdentityWithContext extends CFilter {
    var context : Option[Context] = None
    def isApplied : Boolean = !(None.equals(context))
    def apply(s : CPSStream, c : Context) : CPSStream = {
      context = Some(c)
      s
    }
  }
  /**
   * The basic Composition Operator.
   * Executes first the left operand.
   * If the result given by the left operand is positive,
   * then the right operand is executed on the result and on the context returned by the left operand.
   *
   * examples:
   *
   *
   */

  final class ComposeOperator(left : =>ChainedTransformRoot, right : =>ChainedTransformRoot) extends BinaryOperator(left, right) {
    def metaProcess(metaProcessor : MetaProcessor) =
              metaProcessor.processBinaryOperator(this, new ComposeOperator(_, _), left, right)
    lazy val leftRealized : ChainedTransformRoot = left
    lazy val rightRealized : ChainedTransformRoot = right
    @inline private def forceStream(result: CPSStream, identitySuccess: CFilterIdentityWithContext, identityFailure: CFilterIdentityWithContext) {
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
   * A Composition Operator that does not keep the context
   * Executes first the left operand.
   * If the result given by the left operand is positive,
   * then the right operand is executed on the result by the left operand.
   * Context is reset.
   * examples:
   *
   *
   */

  final class ContextFreeComposeOperator(left : =>ChainedTransformRoot, right : =>ChainedTransformRoot) extends BinaryOperator(left, right) {
    def metaProcess(metaProcessor : MetaProcessor) =
      metaProcessor.processBinaryOperator(this, new ComposeOperator(_, _), left, right)
    lazy val leftRealized : ChainedTransformRoot = left
    lazy val rightRealized : ChainedTransformRoot = right

    def apply(success : =>CFilter, failure : =>CFilter) : CFilter = {
      (tl : CPSStream, c : Context) => {
        val identitySuccess = new CFilterIdentity()
        val result = leftRealized(identitySuccess, failure)(tl, c)
        rightRealized(success, failure)(result, c)
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
    def apply(success : =>CFilter, failure : =>CFilter) : CFilter = leftRealized({
      success
    }, {
      rightRealized(success, failure)
    })
  }

  /**
   * MultiChoiceOperator
   */
  abstract class MultiChoiceOperator(map : Map[String, ()=>ChainedTransformRoot]) extends MultiOperator(map) {
    def extract(context : Context) : String
    def apply(success : =>CFilter, failure : =>CFilter) : CFilter =
                (s : CPSStream, c : Context) => map.apply(extract(c))()(success, failure)(s, c)
  }

  /**
   * Base class for all cardinality operators.
   */
  abstract class CardinalityOperator(underlying : =>ChainedTransformRoot) extends UnaryOperator(underlying)
  
  /**
   * Cardinality operator 1..*
   * Apply repeatedly as many times as possible but at least once the __underlying transform__.
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
   * Apply at most once the underlying transform.
   */
  final class ZeroOrOneOperator(underlying : =>ChainedTransformRoot) extends CardinalityOperator(underlying) {
    def metaProcess(metaProcessor : MetaProcessor) =
              metaProcessor.processUnaryOperator(this, new ZeroOrOneOperator(_), underlying)
    lazy val underlyingRealized : ChainedTransformRoot = underlying
    def apply(success : =>CFilter, failure : =>CFilter) : CFilter = {
      underlyingRealized(success, CPSStreamHelperMethods.appendPositive(success))
    }
  }

  /**
   * Cardinality operator 0..*
   * Apply repeatedly as many times as possible the __underlying transform__.
   */
  final class ZeroOrMoreOperator(underlying : =>ChainedTransformRoot) extends CardinalityOperator(underlying) {
    def metaProcess(metaProcessor : MetaProcessor) =
              metaProcessor.processUnaryOperator(this, new ZeroOrMoreOperator(_), underlying)
    lazy val underlyingRealized : ChainedTransformRoot = underlying
    lazy val sequenceOperator = new SequenceOperator(underlyingRealized, this)
    def apply(success : =>CFilter, failure : =>CFilter) : CFilter = {
      sequenceOperator(success, CPSStreamHelperMethods.appendPositive(success))
    }
  }

  /**
   * Repeat a transform so it matches the input stream at least minOccur times and to the most maxOccur.
   * if maxOccur is less than minOccur then it matches exactly minOccur times.
   */
  def repeat(transform : ChainedTransformRoot, minOccurs : Int, maxOccurs : Int = -1) : ChainedTransformRoot = {
    val constStream = Stream.continually(transform).take(minOccurs)
    val optionalLength= maxOccurs - minOccurs
    val allStream = if (optionalLength > 0)
                      constStream.append(Stream.continually(transform?).take(optionalLength))
                    else
                      constStream

    allStream reduce(
      (x : ChainedTransformRoot, y : ChainedTransformRoot) => new SequenceOperator(x,  y) )
  }

  /**
   * A transform that's context-free
   * (Not using the context, either as input or output)
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
  object drop extends ContextFreeTransform {
    def metaProcess(metaProcessor : MetaProcessor) =
              metaProcessor.processTransform(this, () => { this })
    
    def partialapply(s : CPSStream) : CPSStream = {
      if (CPSStreamHelperMethods.isPositive(s))
        CPSStreamHelperMethods.appendPositiveStream(s.dropWhile(_._2))
      else
        s
    }
  }

  /**
   * an AbstractStatefulSelector performs a selection based on a state.
   * @tparam State A type representing the current state.
   */
  abstract class AbstractStatefulSelector[State] extends ContextFreeTransform {
    def conditionToStop(state : State) : Boolean
    def accumulate(state : State, element : CPSElementOrPositive) : State
    def initialState : State
    def recurse(in : CPSStream, currentState : State) : CPSStream
    override def partialapply(in : CPSStream) : CPSStream = recurse(in, initialState)
  }
  /**
   * This is a template transform to select in the stream using an accumulator.
   */
  abstract class StatefulSelector[State] extends AbstractStatefulSelector[State] {
    final def recurse(in : CPSStream, currentState : State) : CPSStream = {
      if (in.isEmpty)
        Stream.empty
      else
      if (conditionToStop(currentState)) {
        in
      } else
        Stream.cons( (in.head._1, true),
                    recurse(in.tail, accumulate(currentState, in.head._1)))
    }
  }

  /**
   * This is a template transform to select in the stream using an accumulator.
   */
  abstract class StatefulSelectorUntil[State] extends AbstractStatefulSelector[State] {
    final def recurse(in : CPSStream, currentState : State) : CPSStream = {
      if (in.isEmpty)
        Stream.empty
      else
        if (conditionToStop(currentState))
          in
        else {
          val computedState = accumulate(currentState, in.head._1)
          if (conditionToStop(computedState))
               in
          else
               Stream.cons( (in.head._1, true),
                      recurse(in.tail, computedState))

        }
    }
  }

  /**
   * Base class to push from context.
   */
  class PushFromContext(val generator : Context => Stream[Element]) extends ContextReadingTransform {
    def metaProcess(metaProcessor: MetaProcessor) = metaProcessor.processTransform(this, () => { this })
    def partialapply(in : CPSStream, context : Context) : CPSStream = {

        CPSStreamHelperMethods.appendPositiveStream((generator(context) map ( x => (Some(x), true)))).append(in)
      }
  }


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
      var currentContext = context

      // Split the stream into the listBuffer.
      var hd : CPSStream = null
      var tl : CPSStream = null
      do  {
        val failureCF = new CFilterIdentityWithContext()
        val successCF = new CFilterIdentityWithContext()
        val result = tokenizerRealized(successCF, failureCF)(currentStream, currentContext)

        val resTuple = result.span(p => p._2)
        if (failureCF.isApplied) {
          hd = Stream.Empty
          tl = currentStream
        } else {
          hd = resTuple._1
          tl = resTuple._2
          currentContext = successCF.context.get
        }
        currentStream = tl
        if (!hd.isEmpty) {
          listBuffer.append(hd)
        }
      } while (!hd.isEmpty)
      currentStream = null
      hd = null

      // Insert all the listBuffer into a TreeMap.
      val treeMap = TreeMap.empty[String, CPSStream] ++ ((listBuffer map (generator(keyExtractorRealized)(_, context))).toIterator)

      // get all values then flatten them into an unique stream.
      val resultStream = treeMap.values.flatten.toStream append tl
      success(resultStream, context)
    }

    private final def generator(keyExtractor : ChainedTransformRoot)(x : CPSStream, context : Context) : (String, CPSStream) = ((keyExtractor(new CFilterIdentity(), new CFilterIdentity())(CPSStreamHelperMethods.turnToTail(x), context).mkString) -> x)
  }
}
