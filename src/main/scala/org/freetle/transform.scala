package org.freetle
import scala._
import scala.Stream
import scala.io.Source
import util.{EvElemEnd, EvElemStart, XMLEvent}
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
 
	
 
	abstract class Operator extends CFilter
 
	abstract class UnaryOperator(underlying : CFilter) extends Operator
 
   	class RepeatUntilNoResultOperator(underlying : CFilter) extends UnaryOperator(underlying : CFilter) {
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

    //  The deepfilter does return the matching end bracket.
 	class DeepFilter extends BaseTransform {
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
 
 	abstract class BinaryOperator(left : CFilter, right :CFilter) extends Operator
  
  /**
   * This is a concat operator. 
   */
   	class ConcatOperator(left : CFilter, right :CFilter) extends BinaryOperator(left : CFilter, right :CFilter) {
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
    class SequenceOperator(left : CFilter, right :CFilter) extends BinaryOperator(left : CFilter, right :CFilter) {
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
    class SimpleComposeOperator(left : CFilter, right :CFilter) extends BinaryOperator(left : CFilter, right :CFilter) {
		override def apply(in : XMLResultStream) : XMLResultStream = left(right(in))
 	}
    
    /**
     * A compose operator where the left operator cannot consumme the tail from the right operator's result.
     */
    class ComposeOperator(left : CFilter, right :CFilter) extends BinaryOperator(left : CFilter, right :CFilter) {
      
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
    
 	class ChoiceOperator(left : CFilter, right :CFilter) extends BinaryOperator(left : CFilter, right :CFilter) {
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
  


 	abstract class BaseTransform extends CFilter {
	  def concat(right : CFilter) = { 
	    new ConcatOperator(this, right)
    }

    def sequence(right : CFilter) = {
      new SequenceOperator(this, right)
    }

    def simpleCompose(right : CFilter) = {
      new SimpleComposeOperator(this, right)
    }
   
	  def compose(right : CFilter) = {
	    new ComposeOperator(this, right)
	  }
   
	  def andThen(right : CFilter) = {
	    new ComposeOperator(right, this)
	  }
   
	  def choice(right : CFilter) = {
	    new ChoiceOperator(this, right)
	  } 	  
 	}

   
	class ZeroTransform extends BaseTransform {	   
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
 
 	class OnceTransform extends BaseTransform {
		override def apply(in : XMLResultStream) : XMLResultStream =
		  if (in.isEmpty) 
			Stream.empty 
		  else {
				Stream.cons(in.head.toResult(), new ZeroTransform().apply(in.tail)) 
		  } 	
	}
  
 	class IdTransform extends BaseTransform {
		override def apply(in : XMLResultStream) : XMLResultStream = in map (_.toResult)
	}  
  
	// Add helpers to build events simply.
	class PushEvent(event :XMLEvent) extends BaseTransform {
		override def apply(in : XMLResultStream) : XMLResultStream = {
			Stream.cons(Result(event), new ZeroTransform().apply(in))
		}
	}
 
	class TakeStartElement(name :String) extends BaseTransform {
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
	class TakeEndElement(name :String) extends BaseTransform {
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
 
 	object StreamSource  {
	    def fromIterator(s: Iterator[Char]): Source = {
    		lazy val it = s
    		new Source {
    			def reset() = fromIterator(s)
    			val iter = it
    		}
	    }
	  
	}
}


