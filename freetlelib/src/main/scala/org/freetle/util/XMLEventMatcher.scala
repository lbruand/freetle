 /*
  * Copyright 2010-2013 Lucas Bruand
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
package org.freetle.util

/**
 * A base class for all matchers.
 */

abstract class XMLEventMatcher extends (XMLEvent => Boolean)

class NeverMatcher extends XMLEventMatcher {
  def apply(event : XMLEvent) : Boolean = false
}

class AlwaysMatcher extends XMLEventMatcher {
  def apply(event : XMLEvent) : Boolean = true
}

/**
 * This can not be used directly because of type erasure.
 */
trait TypeMatcher[+ParamEvent <: XMLEvent] extends XMLEventMatcher {
  def apply(event : XMLEvent) : Boolean = true //event.isInstanceOf[ParamEvent]
}
class EvElemStartTypeMatcher extends TypeMatcher[EvElemStart] {
  override def apply(event : XMLEvent) : Boolean = event.isInstanceOf[EvElemStart]
}
class EvElemEndTypeMatcher extends TypeMatcher[EvElemEnd] {
  override def apply(event : XMLEvent) : Boolean = event.isInstanceOf[EvElemEnd]
}
class EvTextTypeMatcher extends TypeMatcher[EvText] {
  override def apply(event : XMLEvent) : Boolean =
    event.isInstanceOf[EvText]
}
class EvEntityRefTypeMatcher extends TypeMatcher[EvEntityRef] {
  override def apply(event : XMLEvent) : Boolean = event.isInstanceOf[EvEntityRef]
}
class EvProcInstrTypeMatcher extends TypeMatcher[EvProcInstr] {
  override def apply(event : XMLEvent) : Boolean = event.isInstanceOf[EvProcInstr]
}
class EvCommentTypeMatcher extends TypeMatcher[EvComment] {
  override def apply(event : XMLEvent) : Boolean = event.isInstanceOf[EvComment]
}

abstract class FilterMatcher[+ParamEvent <: XMLEvent] extends TypeMatcher[ParamEvent] {
  def apply[ParamEvent](event : ParamEvent) : Boolean
  override def apply(event : XMLEvent) : Boolean = event match {
    case ev : ParamEvent => apply[ParamEvent](ev)
    case _ => false
  }
}

class OrMatcher(t1 : XMLEventMatcher, t2 : XMLEventMatcher) extends XMLEventMatcher {
  def apply(event : XMLEvent) : Boolean = t1(event) || t2(event)
}

class AndMatcher(t1 : XMLEventMatcher, t2 : XMLEventMatcher) extends XMLEventMatcher {
  def apply(event : XMLEvent) : Boolean = t1(event) && t2(event)
}

abstract class FilterTextMatcher extends FilterMatcher[EvText] {
  def filter : (String => Boolean)
  def apply[EvText](event : EvText) : Boolean = event match { // Very inelegant but filter(event.text) does not seem to work.
    case EvText(text : String) => filter(text)
    case _ => false
  }
}
class SpaceMatcher extends FilterTextMatcher {
  final def filter =  x => "".equals(x.trim())
}

class SpaceOrCommentMatcher extends OrMatcher(new SpaceMatcher(), new EvCommentTypeMatcher())

