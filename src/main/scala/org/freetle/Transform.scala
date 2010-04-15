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
//    =>  There is a need for a Repeat which always succeed...
//  Functionalities
//  ===============
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
//  * Add a offset and size system .
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
  


trait Transform[Context] extends TransformModel[Context] {



  case class RepeatUntilNoResultOperator(override val underlying : CFilterBase) extends
        UnaryOperator(underlying : CFilterBase) {
    def clone(underlying: CFilterBase) = new RepeatUntilNoResultOperator(underlying)

    def recurse(in : XMLResultStream, applied : Boolean) : XMLResultStream = {
			  val result = if (applied) underlying(in) else in
			  if (result.isEmpty) 
				  Stream.empty
			  else {
				  (result.head) match {
				    case Result(_, _) => Stream.cons(result.head, this.recurse(result.tail, false)) // /!\ not tail recursive ?
				    case Tail(_, _) => if (applied)
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
 

  
  /**
   * This is a concat operator. 
   */
  case class ConcatOperator(override val left : CFilterBase, override val right :CFilterBase) extends
        BinaryOperator(left : CFilterBase, right :CFilterBase) {
    def clone(left : CFilterBase, right :CFilterBase) = new ConcatOperator(left, right)

    def recurse(in : XMLResultStream) : XMLResultStream = {
		  if (in.isEmpty)
			  Stream.empty
		  else {
			  (in.head) match {
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
     */
  case class SequenceOperator(override val left : CFilterBase, override val right :CFilterBase) extends
        BinaryOperator(left : CFilterBase, right :CFilterBase) {
    def clone(left : CFilterBase, right :CFilterBase) = new SequenceOperator(left, right)
		def recurse(in : XMLResultStream, hasResult : Boolean) : XMLResultStream = {
		  if (in.isEmpty)
			  Stream.empty
		  else {
			  (in.head) match {
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
			  val resultLeft = left(in)
			  recurse(resultLeft, false)
		  }
		}
 	}
  /**
     * A compose operator where the left operator can consumme the tail from the right operator's result.
     */
  case class SimpleComposeOperator(override val left : CFilterBase, override val right :CFilterBase) extends
        BinaryOperator(left : CFilterBase, right :CFilterBase) {
    def clone(left : CFilterBase, right :CFilterBase) = new SimpleComposeOperator(left, right)
		override def apply(in : XMLResultStream) : XMLResultStream = left(right(in))
 	}
    
  /**
     * A compose operator where the left operator cannot consumme the tail from the right operator's result.
     */
  case class ComposeOperator(override val left : CFilterBase, override val right :CFilterBase) extends
        BinaryOperator(left : CFilterBase, right :CFilterBase) {
    def clone(left : CFilterBase, right :CFilterBase) = new ComposeOperator(left, right)
    def recurseKeepResult(in : XMLResultStream) : XMLResultStream = {
    	  if (in.isEmpty)
			  Stream.empty
		  else {
			  (in.head) match {
			    case Result(_, _) => Stream.cons(in.head, recurseKeepResult(in.tail))
			    case Tail(_, _) => Stream.empty
			  }
          }
    	}
     
    def recurseKeepTail(in : XMLResultStream) : XMLResultStream = {
    	  if (in.isEmpty)
			  Stream.empty
		  else {
			  (in.head) match {
			    case Result(_, _) => recurseKeepTail(in.tail)
			    case Tail(_, _) => in
			  }
          }
    	}
		override def apply(in : XMLResultStream) : XMLResultStream = {
		  val rightStream = right(in)
		  val rightResult = recurseKeepResult(rightStream)
		  Stream.concat(left(rightResult), recurseKeepTail(rightStream))		  
		}	 				
 	}
    
 	case class ChoiceOperator(override val left : CFilterBase, override val right :CFilterBase) extends
        BinaryOperator(left : CFilterBase, right :CFilterBase) {
    def clone(left : CFilterBase, right :CFilterBase) = new ChoiceOperator(left, right)
		override def apply(in : XMLResultStream) : XMLResultStream = {
			val leftResult = left(in)
		  	if (!leftResult.isEmpty)
		       leftResult.head match {
		         case Tail(_, _) => right(in)
		         case Result(_, _) => leftResult		         
		       } 
		    else {
		    	right(in) // Sure this is right if the leftResult stream is empty ?
		    }
		}
 	}
  




   

  /**
   * Base class for all transformations that only output events.
   */
  abstract case class PushTransform extends BaseTransform
  
	// Add helpers to build events simply.
	case class PushEvent(event :event) extends PushTransform {
		override def apply(in : XMLResultStream) : XMLResultStream = {
      val context = if (in.isEmpty) None else in.head.context
			Stream.cons(Result(event, context), new ZeroTransform().apply(in))
		}
	}

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

  case class PushNode(nodeSeq: NodeSeq) extends PushTransform {

    def serializeXML(nodeSeq : NodeSeq, context : Option[Context]) : XMLResultStream = {
      nodeSeq.foldLeft[XMLResultStream](Stream.empty)(
        (x : XMLResultStream, y : Node) => Stream.concat(serializeXML(y, context), x))
    }

    def serializeXML(node : Node, context : Option[Context]) : XMLResultStream = node match {
      case Elem(prefix, label, attributes, scope, child)
            => Stream.cons( Result(new EvElemStart(prefix, label, attributes, scope), context),
                          Stream.concat(serializeXML(child, context),
                            Stream(Result(new EvElemEnd(prefix, label), context))))
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

 	abstract case class BaseTransform extends CFilterBase {
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
   * base transform for all transformations that only take stuff.
   */
  abstract case class TakeTransform extends BaseTransform

  case class ZeroTransform extends TakeTransform {
		override final def apply(in : XMLResultStream) : XMLResultStream = { // Simplier but much more expansive code = ( in map (x => Tail(x.sub)) )
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

  //  The deepfilter does return the matching end bracket.
  case class DeepFilter extends TakeTransform {
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




 	case class IdTransform extends TakeTransform {
		override def apply(in : XMLResultStream) : XMLResultStream = in map (_.toResult)
	}

  case class OnceTransform extends TakeTransform {
		override def apply(in : XMLResultStream) : XMLResultStream =
		  if (in.isEmpty)
			Stream.empty
		  else {
				Stream.cons(in.head.toResult(), new ZeroTransform().apply(in.tail))
		  }
	}
  // TODO There is a problem with the fact that the context is mutable. (Really ?)
  abstract case class TakeDataToContext() extends TakeTransform {

    def pushToContext(text : String, context : Context) : Context

		override def apply(in : XMLResultStream) : XMLResultStream =
		  if (in.isEmpty)
			Stream.empty
		  else in.head.sub match {
        case EvText(text) => {
          val context =
                    in.head.context match {
                      case Some(context) => {
                          Some(pushToContext(text, context))
                      }
                      case None => None
                    }
          Stream.cons( Result(in.head.sub, context), new ZeroTransform().apply(in.tail))
        }
        case _ => new ZeroTransform().apply(in)
      }
  }

	case class TakeStartElement(name :String) extends TakeTransform {
		override def apply(in : XMLResultStream) : XMLResultStream =
			if (in.isEmpty) 
				Stream.empty
			else
		  		in.head.sub match {
		  		  case EvElemStart(_, label : String, _, _)  if (label.equals(name)) 
		  			  	 => Stream.cons(in.head.toResult(), new ZeroTransform().apply(in.tail))
		  		  case _ => new ZeroTransform().apply(in)
		  		} 
	}

	case class TakeEndElement(name :String) extends TakeTransform {
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

  case class TakeText extends TakeTransform {
    def apply(in : XMLResultStream) : XMLResultStream = {
			if (in.isEmpty)
				Stream.empty
			else
		  		in.head.sub match {
            case EvText(_) => Stream.cons(in.head.toResult(), new ZeroTransform().apply(in.tail))
		  		  case _ => new ZeroTransform().apply(in)
		  		}
    }    
  }

  case class TakeSpace extends TakeTransform {
    
    // TODO : There is still a big problem : A text token can be split in many.
    //        And by comments which we might not get every whitespace.
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


