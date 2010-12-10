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

import java.io.InputStream
import util._

/**
 * This is a streaming Continuation Passing Transformation model.
 * It is capable of working over XML Events.
 */

class CPSXMLModel[Context] extends CPSModel[XMLEvent, Context] {
  /**
   * Used for backward compatibility.
   */
  type XMLResultStream = CPSStream

  /**
   * Take all the underlying nodes of the current event.
   * The deepfilter does return the matching end bracket.
   */
  class DeepFilter extends StatefulSelector[Int] {
    def metaProcess(metaProcessor: MetaProcessor) = metaProcessor.processTransform(this, () => { new DeepFilter() })
    def conditionToStop(depth: Int) = depth < 0

    def accumulate(depth: Int, element: CPSElementOrPositive) : Int = depth + (element match {
      case Some(EvElemStart(_, _)) => +1
      case Some(EvElemEnd(_)) => -1
      case _ => 0
    })
    
    def initialState = 1
  }

  abstract class TakeTextToContext extends ContextWritingTransform {
    def metaProcess(metaProcessor: MetaProcessor) = metaProcessor.processTransform(this, () => { this })
    
    def apply(s : CPSStream, c : Context) : (CPSStream, Context) = {
      if (s.isEmpty)
        (s, c)
      else {
        val sr = CPSStreamHelperMethods.removeWhileEmptyPositive(s)
        (sr.head._1.get) match {
          case EvText(txt) =>
            (Stream.cons( (sr.head._1, true), sr.tail), pushToContext(txt, c)) 
          case _ => (s, c)
        }
      }
    }

    def pushToContext(text : String, context : Context) : Context
  }

  /**
   * Shortcut to take an opening tag based on the localpart.
   */
  object < {
    def evStartEvMatcher(name : String)(event : XMLEvent) = {
      event match {
        case EvElemStart(testName,_) if (name.equals(testName.localPart)) => true
        case _ => false
      }
    }
    def apply(name : String) = {
      new ElementMatcherTaker(evStartEvMatcher(name))
    }
  }

  /**
   * Shortcut to take a closing tag based on the localpart.
   */
  object </ {
    def evEndEvMatcher(name : String)(event : XMLEvent) = {
      event match {
        case EvElemEnd(testName) if (name.equals(testName.localPart)) => true
        case _ => false
      }
    }
    def apply(name : String) = {
      new ElementMatcherTaker(evEndEvMatcher(name))
    }
  }
  /**
   * Shortcut to take text.
   */
  object TakeText {
    def apply() = {
      new ElementMatcherTaker(new EvTextTypeMatcher())
    }
  }

  /**
   * Shortcut to take space or comment.
   */
  object TakeSpace {
    def apply() = {
      new ElementMatcherTaker(new SpaceOrCommentMatcher())
    }
  }
  /**
   * Util class to build XMLResultStream, save etc...
   */

  object XMLResultStreamUtils {
    def loadXMLResultStream(in : InputStream, context : Option[Context]) : XMLResultStream = {
      Stream.fromIterator(new XMLEventStream(in) map (x => (Some(x), false)))
    }
    def loadXMLResultStream(str : String, context : Option[Context]) : XMLResultStream = {
      val src = StreamSource.fromIterator(str.toStream.iterator)
      Stream.fromIterator(new XMLEventStream(src) map (x => (Some(x), false)))
    }

    def serializeXMLResultStream(evStream : =>XMLResultStream) : Stream[Char] = {
      (evStream map (_._1.get.toStream)).flatten
    }
  }
  
}