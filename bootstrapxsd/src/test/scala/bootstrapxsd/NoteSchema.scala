package bootstrapxsd
import org.freetle.CPSXMLModel
import org.freetle.meta.CPSMeta
class NoteSchemaContext {

}
class NoteSchema extends CPSXMLModel[NoteSchemaContext] with CPSMeta[NoteSchemaContext] {
  class NoteType extends TransformBase {
    var list = scala.collection.mutable.ArrayBuffer.empty[ChainedTransformRoot]
    def elementTo = <("to") ~ takeText ~ </("to")
    list += elementTo

    def elementFrom = <("from") ~ takeText ~ </("from")
    list += elementFrom

    def elementHeading = <("heading") ~ takeText ~ </("heading")
    list += elementHeading

    def elementBody = <("body") ~ takeText ~ </("body")
    list += elementBody

    override def apply(s : CPSStream, c : NoteSchemaContext) : (CPSStream, NoteSchemaContext) = {
      (s, c)
    }
  }


  def elementNote = <("note") ~ new NoteType() ~ </("note")

}