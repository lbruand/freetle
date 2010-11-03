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
