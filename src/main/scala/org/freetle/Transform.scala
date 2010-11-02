package org.freetle
import annotation.tailrec
import scala.Stream
import util._
import scala.xml._
import javax.xml.namespace.QName


// RAF :

//  Model
//  =====
//  * Stop using prefixes and use namespace instead.
//
//  Functionalities
//  ===============
//  * Write a test checking that the Compose Operator (i.d. irish composition) works properly. (tail  of the first function)
//  * Check that a SAXPath expression can be created using the unitary transformations and operator.
//  * Is it possible to convert the framework into a real pipeline (with multiple threads) ?
//  * Maybe there is a need for a unix 'tee' operator --> Problem how to determine which handside has consummed the more tokens ?
//			(idea) : We could use a element counting Stream
//  * There is some thinking to be done on how to maybe integrate the TakeTransform with the unapply methods
//    and or wildcard cases and or a visitor pattern.
//  * EvPositiveElement trashing is not satisfactory : We need to introduce a pattern for Result checking that
//    encapsulate the EvPositiveElement trashing rather than have this code so widely occuring in the code.
//  * With the namespaces, there is a problem. When you push tokens, how do u know which prefix to use ?
//  * Sorting !!! Introduce a cache.
//
//	Optimizer
//  =========
//  * Write an optimizer that capable of optimising the parser. (Define the rules that might be useful)
//  * There is a need for n-ary operator to help for the optimizer.
//  * Notably, in the case of a choice n-ary operator, it is well possible to optimize a lot:
//	* By using a HashMap on some well chosen key (e.g. the name of the first tag), rather than a sequence of if.
//  * Optimizer need to be capable of factorising:
//      (<body><message>{message}</message></body>)|(<body><operation>{message}</operation></body>)
// 		should be factorised into :
//      <body>(<message>{message}</message>)|(<operation>{message}</operation>)</body>
//			Which is promisingly faster.
  

@serializable @SerialVersionUID(599494944949L + 10 *0L)
trait Transform[Context] extends TransformModel[Context] {
  /**
   * Base class for all transforms.
   */
  @serializable @SerialVersionUID(599494944949L + 10 *1L)
  abstract class CFilterBase extends CFilter {
    def concat(right : CFilterBase) : CFilterBase = {
      new ConcatOperator(this, right)
    }

    def sequence(right : CFilterBase) : CFilterBase = {
      new SequenceOperatorNoBacktrack(this, right)
    }

    def simpleCompose(right : CFilterBase) : CFilterBase = {
      new SimpleComposeOperator(this, right)
    }

    def compose(right : CFilterBase) : CFilterBase= {
      new ComposeOperator(this, right)
    }

    def andThen(right : CFilterBase) : CFilterBase= {
      new ComposeOperator(right, this)
    }

    def choice(right : CFilterBase) = {
      new ChoiceOperator(this, right)
    }

    def repeat : CFilterBase = new RepeatUntilNoResultOperator(this)

    def repeatOrNo : CFilterBase = new WhileNoResultOperator(this)

    def atMostOnce : CFilterBase = new AtMostOnceResultOperator(this)

    def + : CFilterBase = repeat
    def * : CFilterBase = repeatOrNo
    def ? : CFilterBase = atMostOnce

    def ~(right : CFilterBase) = sequence(right)

    def ~~(right : CFilterBase) = concat(right)

    def @@(right : CFilterBase) = compose(right)

    def @@@(right : CFilterBase) = simpleCompose(right)

    def ->(right : CFilterBase) = andThen(right)

    def |(right : CFilterBase) = choice(right)    
  }

  /**
   * Base class for all operators.
   */
  @serializable @SerialVersionUID(599494944949L + 10 *2L)
	abstract class Operator extends CFilterBase

  /**
   * Base class for all unary operators.
   */
  @serializable @SerialVersionUID(599494944949L + 10 *3L)
	abstract case class UnaryOperator(underlying : CFilterBase) extends
        Operator {
    def clone(underlying : CFilterBase) : UnaryOperator
  }

  /**
   * Base class for all binary operators.
   */
  @serializable @SerialVersionUID(599494944949L + 10 *4L)
 	abstract case class BinaryOperator(left : CFilterBase, right :CFilterBase) extends
        Operator {
     def clone(left : CFilterBase, right :CFilterBase) : BinaryOperator
  }

  /**
   * Abstract base class for all cardinality operators.
   */
  @serializable @SerialVersionUID(599494944949L + 10 *5L)
  abstract case class AbstractRepeatOperator(override val underlying : CFilterBase) extends
        UnaryOperator(underlying : CFilterBase) {

    def keepResultWhenEmpty : Boolean
    
    @tailrec 
    protected final def recurse(in : XMLResultStream, applied : Boolean) : XMLResultStream = {
      val result = if (applied) underlying(in) else in
      if (result.isEmpty)
        Stream.empty
      else {
        (result.head) match {
          case Result(EvPositiveResult(), _) => this.recurse(result.tail, false) // --> EvPositiveResult can be trashed.
          case Result(_, _) => Stream.cons(result.head, this.recurse(result.tail, false)) // /!\ not tail recursive ?
          case Tail(_, _) => if (applied)
                      if (keepResultWhenEmpty) result else in// Repeating is over since underlying was applied and we have a Tail in the head.
                    else
                      this.recurse(result, true) // Go on recursing.
        }
      }
    }

  }

  /**
   *  Applies the underlying as long as it returns a result.
   * Cardinality : 1..*
   */
  @serializable @SerialVersionUID(599494944949L + 10 *6L)
  case class RepeatUntilNoResultOperator(override val underlying : CFilterBase) extends
        AbstractRepeatOperator(underlying : CFilterBase) {
    
    def clone(underlying: CFilterBase) = copy(underlying = underlying)

    override final def keepResultWhenEmpty = true

    override def apply(in : XMLResultStream) : XMLResultStream = {
      recurse(in, true)
    }
 	}

  /**
   * Executes the underlying operator at most once.
   * Cardinality : 0..1
   */
  @serializable @SerialVersionUID(599494944949L + 10 *7L)
  case class AtMostOnceResultOperator(override val underlying : CFilterBase) extends
        AbstractRepeatOperator(underlying : CFilterBase) {

    def clone(underlying: CFilterBase) = copy(underlying = underlying)


    override final def keepResultWhenEmpty = true // Unused

    override def apply(in : XMLResultStream) : XMLResultStream = {
      if (in.isEmpty)
        in
      else {
          val result = underlying(in)
          (result.head) match {            
            case Result(_, _) => result
            case Tail(_, context) => Stream.cons(Result(EvPositiveResult(), context), result)
          }
        }
    }
  }

  /**
   * Executes the underlying operator
   * Cardinality : 0..*
   */
  @serializable @SerialVersionUID(599494944949L + 10 *8L)
  case class WhileNoResultOperator(override val underlying : CFilterBase) extends
        AbstractRepeatOperator(underlying : CFilterBase) {

    def clone(underlying: CFilterBase) = copy(underlying = underlying)

    override def keepResultWhenEmpty = false

    override def apply(in : XMLResultStream) : XMLResultStream = {
      val result = recurse(in, true)
      if (result.isEmpty) {
        Stream.empty
      } else {
        (result.head) match {
          case Result(_, _) => result
          case Tail(_, context) => Stream.cons(Result(EvPositiveResult(), context), result)          
        }
      }
    }
  }

  /**
   *  This is a concat operator. 
   * It applies sequentially the left transform first and then right transform, whatever the result of left
   * transform was. 
   */
  @serializable @SerialVersionUID(599494944949L + 10 *9L)
  case class ConcatOperator(override val left : CFilterBase, override val right :CFilterBase) extends
        BinaryOperator(left : CFilterBase, right :CFilterBase) {
    def clone(left : CFilterBase, right :CFilterBase) = copy(left = left, right = right)

    @tailrec
    private final def recurse(in : XMLResultStream) : XMLResultStream = {
		  if (in.isEmpty)
			  Stream.empty
		  else {
			  (in.head) match {
          case Result(EvPositiveResult(), _) => this.recurse(in.tail) // --> EvPositiveResult can be trashed.
			    case Result(_, _) => Stream.cons(in.head, this.recurse(in.tail))
			    case Tail(_, _) => right(in)
			  }
          }
		}
		override def apply(in : XMLResultStream) : XMLResultStream = {
		  if (in.isEmpty)
			  Stream.empty
		  else {
			  val resultLeft = left(in)
			  recurse(resultLeft)
		  }
		}
 	}
    
  /**
   * We execute in sequence left and then right if left has returned a result.
   * does not allow for backtracking decisions. 
   */
  @serializable @SerialVersionUID(599494944949L + 10 *10L)
  case class SequenceOperatorNoBacktrack(override val left : CFilterBase, override val right :CFilterBase) extends
        BinaryOperator(left : CFilterBase, right :CFilterBase) {
    def clone(left : CFilterBase, right :CFilterBase) = copy(left = left, right = right)

    @tailrec
		private final def recurse(in : XMLResultStream, hasResult : Boolean) : XMLResultStream = {
		  if (in.isEmpty)
			  Stream.empty
		  else {
			  (in.head) match {
          case Result(EvPositiveResult(), _) => this.recurse(in.tail, true) // --> EvPositiveResult can be trashed.
			    case Result(_, _) => Stream.cons(in.head, this.recurse(in.tail, true))
			    case Tail(_, _) => if (hasResult) 
			    					right(in) 
			    				else 
			    					in
			  }
      }
		}
    
		override def apply(in : XMLResultStream) : XMLResultStream = {
		  if (in.isEmpty)
			  Stream.empty
		  else { 
			  recurse(left(in), false)
		  }
		}
 	}

    /**
   * We execute in sequence left and then right if left has returned a result.
   * does allow for backtracking decisions.
   * This might prove expensive. 
   */
  @serializable @SerialVersionUID(599494944949L + 10 *11L)
  case class SequenceOperator(override val left : CFilterBase, override val right :CFilterBase) extends
        BinaryOperator(left : CFilterBase, right :CFilterBase) {
    def clone(left : CFilterBase, right :CFilterBase) = copy(left = left, right = right)

    @tailrec
		private final def recurse(current : XMLResultStream, hasResult : Boolean, in : XMLResultStream) : XMLResultStream = {
		  if (current.isEmpty)
			  Stream.empty
		  else {
			  (current.head) match {
          case Result(EvPositiveResult(), _) => this.recurse(current.tail, true, in) // --> EvPositiveResult can be trashed.
			    case Result(_, _) => Stream.cons(current.head, this.recurse(current.tail, true, in))
			    case Tail(_, _) => if (hasResult) {
                                val rightResult = right(current)
                                if (rightResult.isEmpty)
                                  in
                                else
                                  (rightResult.head) match {
                                    case Result(_, _) => rightResult
                                    case Tail(_, _) => in  // No result, we track back to the original Stream. 
                                  }
                             } else
                                current
			  }
      }
		}
		override def apply(in : XMLResultStream) : XMLResultStream = {
		  if (in.isEmpty)
			  Stream.empty
		  else {
			  recurse(left(in), false, in)
		  }
		}
 	}
  
  /**
     * A compose operator where the left operator can consumme the tail from the right operator's result.
     */
  @serializable @SerialVersionUID(599494944949L + 10 *12L)
  case class SimpleComposeOperator(override val left : CFilterBase, override val right :CFilterBase) extends
        BinaryOperator(left : CFilterBase, right :CFilterBase) {
    def clone(left : CFilterBase, right :CFilterBase) = copy(left = left, right = right)
		override def apply(in : XMLResultStream) : XMLResultStream = left(right(in))
 	}
    
  /**
     * A compose operator where the left operator cannot consumme the tail from the right operator's result.
     */
  @serializable @SerialVersionUID(599494944949L + 10 *13L)
  case class ComposeOperator(override val left : CFilterBase, override val right :CFilterBase) extends
        BinaryOperator(left : CFilterBase, right :CFilterBase) {
    def clone(left : CFilterBase, right :CFilterBase) = copy(left = left, right = right)

		override def apply(in : XMLResultStream) : XMLResultStream = {
		  val rightStream = right(in)
      val (rightResult, leftResult) = rightStream.span(_.isInstanceOf[Result])
		  Stream.concat(left(rightResult), leftResult)
		}	 				
 	}

  /**
   * Apply the left transform first. If the left transform has return a result
   * then apply the right transform. 
   */
  @serializable @SerialVersionUID(599494944949L + 10 *14L)
 	case class ChoiceOperator(override val left : CFilterBase, override val right :CFilterBase) extends
        BinaryOperator(left : CFilterBase, right :CFilterBase) {
    def clone(left : CFilterBase, right :CFilterBase) = copy(left = left, right = right)
		override def apply(in : XMLResultStream) : XMLResultStream = {
			val leftResult = left(in)
		  	if (!leftResult.isEmpty)
		       leftResult.head match {
		         case Tail(_, _) => right(in)
             case Result(EvPositiveResult(), _) => leftResult.tail
		         case Result(_, _) => leftResult		         
		       } 
		    else {
		    	right(in) // Sure this is right if the leftResult stream is empty ?
		    }
		}
 	}

  /**
   * Sort operator - Naive implementation for now - stores everything in memory for now.
   * TODO Make this use a disk cache to scale with data.
   * left : group-selector
   * right : select any expression to sort on for the group.
   */
  @serializable @SerialVersionUID(599494944949L + 10 *14L + 1)
  case class SortOperator(override val left : CFilterBase, override val right :CFilterBase) extends
        BinaryOperator(left : CFilterBase, right :CFilterBase) {
    def clone(left : CFilterBase, right :CFilterBase) = copy(left = left, right = right)

    // TODO make this lazy!!!
    @tailrec
    private def recurse(resStream : Stream[XMLResultStream], tail : XMLResultStream)
              : (Stream[XMLResultStream], XMLResultStream)  = {
      val (hd, tl) = left(tail) span (_.isInstanceOf[Result])
      if (hd.isEmpty) {
        (resStream, tail)
      } else {
        recurse(Stream.cons(hd, resStream), tl)
      }
    }

    def toString(in : XMLResultStream) : String = {
      in.foldLeft("")( _ + _.subEvent.toString)
    }

    override def apply(in : XMLResultStream) : XMLResultStream = {
      val (hd, tl) = recurse(Stream.empty, in)
      val sortedHead = hd.sortBy[String](x => toString(right(x)) )
      Stream.concat(sortedHead.flatten map (_.toResult), tl)
    }
  }

  /**
   *  Base class for all transformations that only output events and take nothing off the Stream.
   */
  @serializable @SerialVersionUID(599494944949L + 10 *15L)
  abstract case class PushTransform extends BaseTransform
  
	// Add helpers to build events simply.

  /**
   * Push a single event down on the pipeline, not consumming any event in entry.
   */
  @serializable @SerialVersionUID(599494944949L + 10 *16L)
	case class PushEvent[+Event <: event](ev :Event) extends PushTransform {
		override def apply(in : XMLResultStream) : XMLResultStream = {
      val context = if (in.isEmpty) None else in.head.context
			Stream.cons(Result(ev, context), new ZeroTransform().apply(in))
		}
	}

  /**
   * Abstract class to push stuff from the context.
   */
  @serializable @SerialVersionUID(599494944949L + 10 *17L)
  abstract case  class PushFromContext extends PushTransform {
    def generate(context : Context) : event
    override def apply(in : XMLResultStream) : XMLResultStream = {
      if (in.isEmpty)
        Stream.empty
      else {
        val someContext = in.head.context
        someContext match {
          case Some(context) => Stream.cons(Result(generate(context), Some(context)), new ZeroTransform().apply(in))
          case None => in
        }
      }
		}
  }

  /**
   * Abstract class to push a stream of events down the pipeline 
   */
  @serializable @SerialVersionUID(599494944949L + 10 *18L)
  abstract case  class PushSeqFromContext extends PushTransform {
    def generate(context : Context) : Stream[event]
    override def apply(in : XMLResultStream) : XMLResultStream = {
      if (in.isEmpty)
        Stream.empty
      else {
        val someContext = in.head.context
        someContext match {
          case Some(context) => Stream.concat(generate(context).map(Result(_, Some(context))), new ZeroTransform().apply(in))
          case None => in
        }
      }
		}
  }

  /**
   * Push a scala xml content down the pipeline.
   */
  @serializable @SerialVersionUID(599494944949L + 10 *19L)
  case class PushNode(nodeSeq: NodeSeq) extends PushTransform {

    def serializeXML(nodeSeq : NodeSeq, context : Option[Context]) : XMLResultStream = {
      nodeSeq.foldLeft[XMLResultStream](Stream.empty)(
        (x : XMLResultStream, y : Node) => Stream.concat(serializeXML(y, context), x))
    }

    
    /*def buildAttributes(attributes : MetaData) : Map[QName, String] = {
      attributes.
    }*/
    def serializeXML(node : Node, context : Option[Context]) : XMLResultStream = node match {
      case Elem(prefix, label, attributes, scope, child)
            => {
                  val qName: QName = if (prefix == null || prefix.isEmpty)
                                        new javax.xml.namespace.QName(scope.getURI(null), label)
                                     else
                                        new javax.xml.namespace.QName(scope.getURI(prefix), label, prefix)
                  Stream.cons( Result(new EvElemStart(qName,  null), context),
                          Stream.concat(serializeXML(child, context),
                            Stream(Result(new EvElemEnd(qName), context))))
                }
      case Text(text) => Stream(Result(new EvText(text), context))
      case Comment(text) => Stream(Result(new EvComment(text), context))
      case ProcInstr(target, procText) => Stream(Result(new EvProcInstr(target, procText), context))
      case EntityRef(entityRef) => Stream(Result(new EvEntityRef(entityRef), context))
      case node : Node => Stream(Result(new EvText(node.text), context))
    }

    override def apply(in : XMLResultStream) : XMLResultStream = {
        val context = if (in.isEmpty) None else in.head.context
        Stream.concat(serializeXML(nodeSeq, context), in)
      }
  }

  /**
   * Base non-composite transform class.
   * (not composite in the sens it is not requiring any underlying transform,
   * opposing meaning of an operator) 
   */
  @serializable @SerialVersionUID(599494944949L + 10 *20L)
 	abstract case class BaseTransform extends CFilterBase
  /**
   * base transform for all transformations that only take stuff.
   */
  @serializable @SerialVersionUID(599494944949L + 10 *21L)
  abstract case class TakeTransform extends BaseTransform {    
    protected final def selectUntilAccumulation[Accumulator](in : XMLResultStream,
                                             accumulator : Accumulator,
                                             conditionToStop : Accumulator=>Boolean,
                                             accumulation : (Accumulator, TransformResult) => Accumulator)
            : XMLResultStream = {
      if (in.isEmpty)
        Stream.empty
      else
      if (conditionToStop(accumulator))
        in
      else
        Stream.cons(in.head.toResult(),
                    selectUntilAccumulation(in.tail, accumulation(accumulator, in.head), conditionToStop, accumulation))

    }
  }

  /**
   * The ZeroTransform is the simpliest transform. It takes all the input and make equivalent Tailed version.
   * There is a simplier but much more expensive code in the form of apply = ( in map (x => Tail(x.subEvent)) )
   * but it is rather lacking because it does not take advantage of the fact that the stream is 'Tailed' after a
   * certain rank.
   */
  @serializable @SerialVersionUID(599494944949L + 10 *22L)
  case class ZeroTransform extends TakeTransform {
		override final def apply(in : XMLResultStream) : XMLResultStream = {
		  if (in.isEmpty)
			Stream.empty
		  else {
			  in.head match {
			    case Result(_, _) => Stream.cons(in.head.toTail(), this.apply(in.tail))
			    case Tail(_, _) => in // When we have reached the first Tail, the recursion is over.
			  }

		  }

		}
	}

  /**
   * Take all the underlying nodes of the current event.
   * The deepfilter does return the matching end bracket.
   */
  @serializable @SerialVersionUID(599494944949L + 10 *23L)
  case class DeepFilter extends TakeTransform {
    
  def accumulation(depth: Int, in: TransformResult) : Int =
    depth + (in.subEvent match {
      case e: EvElemStart => +1
      case e: EvElemEnd => -1
      case _ => 0
    })
  
  override def apply(in : XMLResultStream) : XMLResultStream = selectUntilAccumulation[Int](in,
                                                                                            0,  // <-- Initial depth 
                                                                                            _ < 0, // <-- When to stop
                                                                                            accumulation)

}

  /**
   * Drop everything from the incoming result.
   */
  @serializable @SerialVersionUID(599494944949L + 10 *24L)
  case class DropFilter extends TakeTransform {
    override def apply(in : XMLResultStream) : XMLResultStream = {
      val result = in.dropWhile( _.isInstanceOf[Result] )
      if (result.isEmpty)
        Stream.empty
      else
        Stream.cons(Result(new EvPositiveResult(), result.head.context), result)
    }
  }

  /**
   * Turn all the stream into the equivalent Identical result stream.
   */
  @serializable @SerialVersionUID(599494944949L + 10 *25L)
 	case class IdTransform extends TakeTransform {
		override def apply(in : XMLResultStream) : XMLResultStream = in map (_.toResult)
	}

  /**
   * Take a text from the entry and store it in the context.
   */
  @serializable @SerialVersionUID(599494944949L + 10 *26L)
  abstract case class TakeDataToContext() extends TakeTransform {
    // TODO There is a problem with the fact that the context is mutable. (Really ?)
    def pushToContext(text : String, context : Context) : Context

		override def apply(in : XMLResultStream) : XMLResultStream =
		  if (in.isEmpty)
			  Stream.empty
		  else in.head.subEvent match {
        case EvText(text) => {
          val newContext =
                in.head.context match {
                  case Some(oldContext) => {
                      Some(pushToContext(text, oldContext))
                  }
                  case None => None
                }
          Stream.cons( Result(in.head.subEvent, newContext), in.tail map (x => Tail(x.subEvent, newContext)))
        }
        case _ => new ZeroTransform().apply(in)
      }
  }

  /**
   * Take one Tail at the head and turn it into a Result
   */
  @serializable @SerialVersionUID(599494944949L + 10 *27L)
  abstract class TakeOnceTransform extends TakeTransform {
    def matcher : XMLEventMatcher
		override def apply(in : XMLResultStream) : XMLResultStream =
		  if (in.isEmpty)
			  Stream.empty
		  else {
        if (matcher(in.head.subEvent))
				  Stream.cons(in.head.toResult(), new ZeroTransform().apply(in.tail))
        else
          new ZeroTransform().apply(in)
		  }
	}
  @serializable @SerialVersionUID(599494944949L + 10 * 28L)
  class TakeAnyTransform extends TakeOnceTransform {
    def matcher : XMLEventMatcher = new AlwaysMatcher()   
  }
  
  /**
   * Take a single start element event if it has the correct name.
   */
  @serializable @SerialVersionUID(599494944949L + 10 * 29L)
	case class TakeStartElement(name : String) extends TakeOnceTransform {
    @serializable @SerialVersionUID(599494944949L + 10 * 29L + 1L)
    final class LabelEvElemStartFilterMatcher(name : String) extends FilterMatcher[EvElemStart]() {
      def apply[EvElemStart](event : EvElemStart) : Boolean = event match {
        case EvElemStart(label : QName, _)  if (label.getLocalPart.equals(name)) => true
        case _ => false
      }
    }
    def matcher = new LabelEvElemStartFilterMatcher(name)
	}

  /**
   * Take a single end element event if it has the correct name (but regardless of its namespace)
   */
  @serializable @SerialVersionUID(599494944949L + 10 * 30L)
  case class TakeEndElement(name : String) extends TakeOnceTransform {
    @serializable @SerialVersionUID(599494944949L + 10 * 30L + 1L)
    final class LabelEvElemEndFilterMatcher(name : String) extends FilterMatcher[EvElemEnd]() {
      def apply[EvElemEnd](event : EvElemEnd) : Boolean = event match {
        case EvElemEnd(label: QName)  if (label.getLocalPart.equals(name)) => true
        case _ => false
      }
    }
    def matcher = new LabelEvElemEndFilterMatcher(name)
	}

  /**
   * Take a single text event.
   */
  @serializable @SerialVersionUID(599494944949L + 10 * 31L)
  case class TakeText extends TakeOnceTransform {
    def matcher = new EvTextTypeMatcher()
  }

  /**
   * Take a whitespace event.
   */
  @serializable @SerialVersionUID(599494944949L + 10 * 32L)
  case class TakeSpace extends TakeOnceTransform {
    def matcher = new SpaceOrCommentMatcher()
  }

  /**
   * Shortcut to create TakeStartElement.
   */
  @serializable @SerialVersionUID(599494944949L + 10 * 33L)
  object < {
    def apply(name : String) = new TakeStartElement(name)
  }
  /**
   * Shortcut to create TakeEndElement.
   */
  @serializable @SerialVersionUID(599494944949L + 10 * 34L)
  object </ {
    def apply(name : String) = new TakeEndElement(name)
  }
}



