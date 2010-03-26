package org.freetle
import scala._
import scala.Stream
import scala.io.Source
import util._
import xml._
// RAF :

//  Model
//  =====
//  * there is no possibility of having an empty positive-result since
//		 Since failure is an empty result. 
// 		We might need a special token that helps with empty positive-result.  
//  Functionalities
//  ===============
//  * Stax Parser fuck up the bad way... Understand why.
//  * Add the operators on the Transform class
//  * Add multi unary operator
//  * Check that the Compose Operator (i.d. irish composition) works properly. (tail  of the first function)
//  * Check that a SAXPath expression can be created using the unitary transformations and operaor.
//  * Check for space-performance.
//  * Is it possible to convert the framework into a real pipeline (with multiple threads) ?
//  * Maybe there is a need for a unix 'tee' operator --> Problem how to determine which handside has consummed the more tokens ?
//			(idea) : We could use a element counting Stream                                                                      
//  * Solve the template output problem : Sometimes it is necessary to reorder/sort a bit element. How to deal with that nicely.
//			Use context definition ?
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
  

object transform {
  
	// ============ Model =====================

    abstract class TransformResult(val sub : XMLEvent) {
      def toTail() : TransformResult = {
        this match {
          case Result(sub) => new Tail(sub)
          case _ => this
        }
      }
      def toResult() : TransformResult = {
        this match {
          case Tail(sub) => new Result(sub)
          case _ => this
        }
      }
    }
    case class Result(override val sub : XMLEvent) extends TransformResult(sub : XMLEvent)
    case class Tail(override val sub : XMLEvent) extends TransformResult(sub : XMLEvent)    
      
    // TODO Find a way to add a constraint on the Stream. (Useful ? ) 
    //		The constraint being : We have Results until a certain rank and then we have Tails. 
	type XMLResultStream = Stream[TransformResult]
    // ============ End of Model ==============	                             
 
 
	type CFilter =
		XMLResultStream => XMLResultStream
 
	abstract class CFilterBase extends CFilter
 
	abstract class Operator extends CFilterBase
 
	abstract class UnaryOperator(val underlying : CFilterBase) extends Operator {
    def clone(underlying : CFilterBase) : UnaryOperator  
  }
 
  class RepeatUntilNoResultOperator(override val underlying : CFilterBase) extends UnaryOperator(underlying : CFilterBase) {
    def clone(underlying: CFilterBase) = new RepeatUntilNoResultOperator(underlying)

    def recurse(in : XMLResultStream, applied : Boolean) : XMLResultStream = {
			  val result = if (applied) underlying(in) else in
			  if (result.isEmpty) 
				  Stream.empty
			  else {
				  (result.head) match {
				    case Result(_) => Stream.cons(result.head, this.recurse(result.tail, false)) // /!\ not tail recursive ?
				    case Tail(_) => if (applied)
				    					  result // Repeating is over since underlying was applied and we have a Tail in the head.
				    				  else 
				    					  this.recurse(result, true) // Go on recursing. 
				  }  
			  } 
			}
			override def apply(in : XMLResultStream) : XMLResultStream = {
			  recurse(in, true)
			}
 	}  
 
 	abstract class BinaryOperator(left : CFilterBase, right :CFilterBase) extends Operator {
     def clone(left : CFilterBase, right :CFilterBase) : BinaryOperator
   }
  
  /**
   * This is a concat operator. 
   */
  class ConcatOperator(left : CFilterBase, right :CFilterBase) extends BinaryOperator(left : CFilterBase, right :CFilterBase) {
    def clone(left : CFilterBase, right :CFilterBase) = new ConcatOperator(left, right)

    def recurse(in : XMLResultStream) : XMLResultStream = {
		  if (in.isEmpty)
			  Stream.empty
		  else {
			  (in.head) match {
			    case Result(_) => Stream.cons(in.head, this.recurse(in.tail))
			    case Tail(_) => right(in)
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
     */
  class SequenceOperator(left : CFilterBase, right :CFilterBase) extends BinaryOperator(left : CFilterBase, right :CFilterBase) {
    def clone(left : CFilterBase, right :CFilterBase) = new SequenceOperator(left, right)
		def recurse(in : XMLResultStream, hasResult : Boolean) : XMLResultStream = {
		  if (in.isEmpty)
			  Stream.empty
		  else {
			  (in.head) match {
			    case Result(_) => Stream.cons(in.head, this.recurse(in.tail, true))
			    case Tail(_) => if (hasResult) 
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
			  val resultLeft = left(in)
			  recurse(resultLeft, false)
		  }
		}
 	}
  /**
     * A compose operator where the left operator can consumme the tail from the right operator's result.
     */
  class SimpleComposeOperator(left : CFilterBase, right :CFilterBase) extends BinaryOperator(left : CFilterBase, right :CFilterBase) {
    def clone(left : CFilterBase, right :CFilterBase) = new SimpleComposeOperator(left, right)
		override def apply(in : XMLResultStream) : XMLResultStream = left(right(in))
 	}
    
  /**
     * A compose operator where the left operator cannot consumme the tail from the right operator's result.
     */
  class ComposeOperator(left : CFilterBase, right :CFilterBase) extends BinaryOperator(left : CFilterBase, right :CFilterBase) {
    def clone(left : CFilterBase, right :CFilterBase) = new ComposeOperator(left, right)
    def recurseKeepResult(in : XMLResultStream) : XMLResultStream = {
    	  if (in.isEmpty)
			  Stream.empty
		  else {
			  (in.head) match {
			    case Result(_) => Stream.cons(in.head, recurseKeepResult(in.tail))
			    case Tail(_) => Stream.empty
			  }
          }
    	}
     
    def recurseKeepTail(in : XMLResultStream) : XMLResultStream = {
    	  if (in.isEmpty)
			  Stream.empty
		  else {
			  (in.head) match {
			    case Result(_) => recurseKeepTail(in.tail)
			    case Tail(_) => in
			  }
          }
    	}
		override def apply(in : XMLResultStream) : XMLResultStream = {
		  val rightStream = right(in)
		  val rightResult = recurseKeepResult(rightStream)
		  Stream.concat(left(rightResult), recurseKeepTail(rightStream))		  
		}	 				
 	}
    
 	class ChoiceOperator(left : CFilterBase, right :CFilterBase) extends BinaryOperator(left : CFilterBase, right :CFilterBase) {
    def clone(left : CFilterBase, right :CFilterBase) = new ChoiceOperator(left, right)
		override def apply(in : XMLResultStream) : XMLResultStream = {
			val leftResult = left(in)
		  	if (!leftResult.isEmpty)
		       leftResult.head match {
		         case Tail(_) => right(in)
		         case Result(_) => leftResult		         
		       } 
		    else {
		    	right(in) // Sure this is right if the leftResult stream is empty ?
		    }
		}
 	}
  


 	abstract class BaseTransform extends CFilterBase {
	  def concat(right : CFilterBase) = {
	    new ConcatOperator(this, right)
    }

    def sequence(right : CFilterBase) = {
      new SequenceOperator(this, right)
    }

    def simpleCompose(right : CFilterBase) = {
      new SimpleComposeOperator(this, right)
    }
   
	  def compose(right : CFilterBase) = {
	    new ComposeOperator(this, right)
	  }
   
	  def andThen(right : CFilterBase) = {
	    new ComposeOperator(right, this)
	  }
   
	  def choice(right : CFilterBase) = {
	    new ChoiceOperator(this, right)
	  } 	  
 	}

   

  /**
   * Base class for all transformations that only output events.
   */
  abstract class PushTransform extends BaseTransform
  
	// Add helpers to build events simply.
	class PushEvent(event :XMLEvent) extends PushTransform {
		override def apply(in : XMLResultStream) : XMLResultStream = {
			Stream.cons(Result(event), new ZeroTransform().apply(in))
		}
	}

  class PushNode(nodeSeq: NodeSeq) extends PushTransform {

    def serializeXML(nodeSeq : NodeSeq) : XMLResultStream = {
      nodeSeq.foldLeft[XMLResultStream](Stream.empty)((x :XMLResultStream, y :Node) => Stream.concat(serializeXML(y), x))
    }

    def serializeXML(node : Node) : XMLResultStream = node match {
      case Elem(prefix, label, attributes, scope, child)
            => Stream.cons( Result(new EvElemStart(prefix, label, attributes, scope)),
                          Stream.concat(serializeXML(child), Stream(Result(new EvElemEnd(prefix, label)))))
      case Text(text) => Stream(Result(new EvText(text)))
      case Comment(text) => Stream(Result(new EvComment(text)))
      case ProcInstr(target, procText) => Stream(Result(new EvProcInstr(target, procText)))
      case EntityRef(entityRef) => Stream(Result(new EvEntityRef(entityRef)))
      case node : Node => Stream(Result(new EvText(node.text)))
    }

    override def apply(in : XMLResultStream) : XMLResultStream =
      Stream.concat(serializeXML(nodeSeq), in)
  }


  /**
   * base transform for all transformations that only take stuff.
   */
  abstract class TakeTransform extends BaseTransform

  class ZeroTransform extends TakeTransform {
		override final def apply(in : XMLResultStream) : XMLResultStream = { // Simplier but much more expansive code = ( in map (x => Tail(x.sub)) )
		  if (in.isEmpty)
			Stream.empty
		  else {
			  in.head match {
			    case Result(_) => Stream.cons(in.head.toTail(), this.apply(in.tail))
			    case Tail(_) => in // When we have reached the first Tail, the recursion is over.
			  }

		  }

		}
	}

  //  The deepfilter does return the matching end bracket.
 class DeepFilter extends TakeTransform {
  def recurseDeep(in : XMLResultStream, depth :Int) : XMLResultStream = {
      if (in.isEmpty)
        Stream.empty
      else
        if (depth < 0)
          in
        else {
          val acc = in.head.sub match {
              case EvElemStart(_, _, _, _) => +1
            case EvElemEnd(_, _) 		 => -1
            case _ 						 =>  0
          }
          Stream.cons(in.head.toResult(), recurseDeep(in.tail, depth + acc))
        }
  }
  override def apply(in : XMLResultStream) : XMLResultStream = {
      recurseDeep(in, 0)
  }
}




 	class IdTransform extends TakeTransform {
		override def apply(in : XMLResultStream) : XMLResultStream = in map (_.toResult)
	}

  class OnceTransform extends TakeTransform {
		override def apply(in : XMLResultStream) : XMLResultStream =
		  if (in.isEmpty)
			Stream.empty
		  else {
				Stream.cons(in.head.toResult(), new ZeroTransform().apply(in.tail))
		  }
	}

	class TakeStartElement(name :String) extends TakeTransform {
		override def apply(in : XMLResultStream) : XMLResultStream =
			if (in.isEmpty) 
				Stream.empty
			else
		  		in.head.sub match {
		  		  case EvElemStart(_, label: String, _, _)  if (label.equals(name)) 
		  			  	 => Stream.cons(in.head.toResult(), new ZeroTransform().apply(in.tail))
		  		  case _ => new ZeroTransform().apply(in)
		  		} 
	}

	class TakeEndElement(name :String) extends TakeTransform {
		override def apply(in : XMLResultStream) : XMLResultStream =
			if (in.isEmpty)
				Stream.empty
			else
		  		in.head.sub match {
		  		  case EvElemEnd(_, label: String)  if (label.equals(name))
		  			  	 => Stream.cons(in.head.toResult(), new ZeroTransform().apply(in.tail))
		  		  case _ => new ZeroTransform().apply(in)
		  		}
	}

  class TakeSpace extends TakeTransform {
    def apply(in : XMLResultStream) : XMLResultStream = {
			if (in.isEmpty)
				Stream.empty
			else
		  		in.head.sub match {		  		  
            case EvComment(text : String)
                  => Stream.cons(in.head.toResult(), new ZeroTransform().apply(in.tail))
            case EvText(text : String) if ("".equals(text.trim()))
                  => Stream.cons(in.head.toResult(), new ZeroTransform().apply(in.tail)) 
		  		  case _ => new ZeroTransform().apply(in)
		  		}
    }
  }
 

}


