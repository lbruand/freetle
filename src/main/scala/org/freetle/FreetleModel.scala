package org.freetle

import util.XMLEvent

/**
 * This is the freetle model classes.
 */
@serializable @SerialVersionUID(599494944949L + 1001L)
trait FreetleModel[Context] {
  // ============ Model =====================
  type event = XMLEvent
  @serializable @SerialVersionUID(599494944949L + 1002L)
  abstract class TransformResult(val subEvent : event, val context : Option[Context]) {
    def toTail() : TransformResult = {
      this match {
        case Result(sub, context) => new Tail(sub, context)
        case _ => this
      }
    }
    def toResult() : TransformResult = {
      this match {
        case Tail(sub, context) => new Result(sub, context)
        case _ => this
      }
    }
  }
  @serializable @SerialVersionUID(599494944949L + 1003L)
  case class Result(override val subEvent : event, override val context : Option[Context])
          extends TransformResult(subEvent : event, context : Option[Context])
  @serializable @SerialVersionUID(599494944949L + 1004L)
  case class Tail(override val subEvent : event, override val context : Option[Context])
          extends TransformResult(subEvent : event, context : Option[Context])

    //		The constraint being : We have Results until a certain rank and then we have Tails.
  type XMLResultStream = Stream[TransformResult]
    // ============ End of Model ==============

}
