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

import util._


/**
 * This model is used to transform a Stream[Char] towards a Stream[XMLEvent]
 */
class CPSTranslateModel[Context] extends CPSModel[Either[Char, XMLEvent], Context] {
  implicit def convert(c : Char) : Either[Char, XMLEvent] = Left(c)
  implicit def convert(c : XMLEvent) : Either[Char, XMLEvent] = Right(c)


  def const(s :String) : ChainedTransformRoot = {
    s map ((input : Char) => new ElementMatcherTaker(
          (x :Either[Char, XMLEvent]) => x match {
            case Left(input) => true
            case _ => false
          }
    )) reduce( (x : ChainedTransformRoot, y : ChainedTransformRoot) => new SequenceOperator(x,  y) )
  }

  /**
   * A base class to load text tokens to context.
   * TODO : Test
   */
  abstract class TakeResultToContext extends ContextWritingTransform {
    def metaProcess(metaProcessor: MetaProcessor) = metaProcessor.processTransform(this, () => { this })

    @inline def apply(stream : CPSStream, context : Context) : (CPSStream, Context) = {
      if (stream.isEmpty)
        (stream, context)
      else {
        //val sr = CPSStreamHelperMethods.removeAllEmptyPositive(stream)
        val (shead, stail) = stream span (x => x._2)
        val sr = CPSStreamHelperMethods.removeAllEmptyPositive(shead)
        val result : String = (sr map ( (x :(Option[Either[Char, XMLEvent]], Boolean)) => x match {
            case (Some(Left(c)), false) => c
            case _ => ' '
          } )).mkString
        (stail, pushToContext(result, context))
      }
    }

    def pushToContext(text : String, context : Context) : Context
  }

  /**
   * A Context-free transform that matches elements.
   * TODO : Remove replace by TakeResultToContext and appropriate context outputing stuff.
   */
  final class ValueTaker(val len : Int, val tag : QName)  extends ContextFreeTransform {
    def metaProcess(metaProcessor : MetaProcessor) =
              metaProcessor.processTransform(this, () => { new ValueTaker(len, tag) })

    @inline def partialapply(s : CPSStream) : CPSStream = {
      if (s.isEmpty)
        s
      else {
        val sr = CPSStreamHelperMethods.removeAllEmptyPositive(s)  // TODO this is not ok.
                                          //  We are adding that too many times.
        val result : Stream[Char] = sr.take(len) map ( (x :(Option[Either[Char, XMLEvent]], Boolean)) => x match {
            case (Some(Left(c)), false) => c
            case _ => ' '
          } )
        (
          Stream(new EvElemStart(tag), new EvText(result.mkString), new EvElemEnd(tag)) map (x => (Some(Right(x)), true))
        ).append(sr.drop(len))
      }
    }
  }
}