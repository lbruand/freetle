package bootstrapxsd
import org.freetle.CPSXMLModel
import org.freetle.meta.CPSMeta


class NoteSchemaContext {}

class NoteSchema extends CPSXMLModel[NoteSchemaContext] with CPSMeta[NoteSchemaContext] {
  abstract class SequenceBaseType extends (()=>ChainedTransformRoot) {
    var list = scala.collection.mutable.ArrayBuffer.empty[ChainedTransformRoot]
    def apply() : ChainedTransformRoot=  list.reduceLeft( (x, y) => new SequenceOperator(x,y) )
  }

  class NoteType extends SequenceBaseType {

    def elementTo = <("to") ~ takeText ~ </("to")
    list += elementTo

    def elementFrom = <("from") ~ takeText ~ </("from")
    list += elementFrom

    def elementHeading = <("heading") ~ takeText ~ </("heading")
    list += elementHeading

    def elementBody = <("body") ~ takeText ~ </("body")
    list += elementBody

  }


  def elementNote = <("note") ~ (new NoteType())() ~ </("note")

}