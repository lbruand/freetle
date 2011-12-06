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

  def repeat(len : Int, transform : ChainedTransformRoot ) : ChainedTransformRoot = {
    Stream.continually(transform).take(len)  reduce(
      (x : ChainedTransformRoot, y : ChainedTransformRoot) => new SequenceOperator(x,  y) )
  }
  val takeAnyChar = new ElementMatcherTaker((x :Either[Char, XMLEvent]) => x match {
            case Left(_) => true
            case _ => false
          })

  def const(s :String) : ChainedTransformRoot = {
    s map ((input : Char) => new ElementMatcherTaker(
          (x :Either[Char, XMLEvent]) => x match {
            case Left(`input`) => true
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
            case (Some(Left(c)), true) => c
            case _ => ' '
          } )).mkString
        (CPSStreamHelperMethods.appendPositiveStream(stail), pushToContext(result, context))
      }
    }

    def pushToContext(text : String, context : Context) : Context
  }

  /**
   * Shortcut to output various objects downstream.
   */
  object > {
    def apply(formatter: Context => String) : PushFormattedText = {
      new PushFormattedText(formatter = formatter)
    }

    def apply(text : String) : PushText = {
      new PushText(text = text)
    }

    def apply(event : XMLEvent) : PushFromContext = new PushFromContext(c => Stream(Right(event)))

    def apply(events :Stream[XMLEvent]) : PushFromContext = new PushFromContext(c => (events map (Right(_))))

    def apply(events :Context => Stream[XMLEvent]) : PushFromContext = new PushFromContext(c => (events(c) map (Right(_))))
  }

  /**
   * Push Formatted text from the context down to output stream.
   */
  class PushFormattedText(formatter: Context => String) extends PushFromContext(
    formatter andThen ((x:String) => Stream(Right(new EvText(x))))
  ) {
    override def metaProcess(metaProcessor: MetaProcessor) = metaProcessor.processTransform(this, () => { this })
  }
  /**
   * Output text downstream.
   */
  class PushText(text: String) extends PushFromContext(x => Stream(Right(new EvText(text)))) {
    override def metaProcess(metaProcessor: MetaProcessor) = metaProcessor.processTransform(this, () => { this })
  }

}