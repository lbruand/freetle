package org.freetle.util

import scala.xml.{MetaData, NamespaceBinding}
import javax.xml.stream.Location

/** This class represents an XML event for pull parsing.
 *  Pull parsing means that during the traversal of the XML
 *  tree we are parsing, each "event" is returned to the caller
 *  and the traversal is suspended.
 */
sealed abstract class XMLEvent {
  var location : Location = null
}

/** An element is encountered the first time */
case class EvElemStart(pre: String, label: String, attrs: MetaData, scope: NamespaceBinding) extends XMLEvent

/** An element is encountered the last time */
case class EvElemEnd(pre: String, label: String) extends XMLEvent
/** A text node is encountered */
case class EvText(text : String) extends XMLEvent

/** An entity reference is encountered */
case class EvEntityRef(entity: String) extends XMLEvent

/** A processing instruction is encountered */
case class EvProcInstr(target: String, text: String) extends XMLEvent

/** A comment is encountered */
case class EvComment(text: String) extends XMLEvent

/** Used when we want a empty but yet positive result */
case class EvPositiveResult() extends XMLEvent



abstract class XMLEventMatcher extends (XMLEvent => Boolean)

class NeverMatcher extends XMLEventMatcher {
  def apply(event : XMLEvent) : Boolean = false
}

class AlwaysMatcher extends XMLEventMatcher {
  def apply(event : XMLEvent) : Boolean = true
}

class TypeMatcher[+ParamEvent <: XMLEvent] extends XMLEventMatcher {
  def apply(event : XMLEvent) : Boolean = event.isInstanceOf[ParamEvent]
}

abstract class FilterMatcher[+ParamEvent <: XMLEvent] extends TypeMatcher[ParamEvent] {
  def apply[ParamEvent](event : ParamEvent) : Boolean
  override def apply(event : XMLEvent) : Boolean = event match {
    case ev : ParamEvent => apply[ParamEvent](ev)
    case _ => false
  }
}

class OrMatcher[+T1 <: XMLEventMatcher, +T2 <: XMLEventMatcher] extends XMLEventMatcher {
  //                                               V : This fucks up big time... The apply function called is not the one believe.  
  def apply(event : XMLEvent) : Boolean = this.asInstanceOf[T1].apply(event) ^ this.asInstanceOf[T2].apply(event)
}

class AndMatcher[+T1 <: XMLEventMatcher, +T2 <: XMLEventMatcher] extends XMLEventMatcher {
  def apply(event : XMLEvent) : Boolean = this.asInstanceOf[T1].apply(event) & this.asInstanceOf[T2].apply(event)
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

class SpaceOrCommentMatcher extends OrMatcher[SpaceMatcher, TypeMatcher[EvComment]]

