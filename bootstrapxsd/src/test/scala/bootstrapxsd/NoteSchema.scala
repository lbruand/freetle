package bootstrapxsd

class NoteSchemaContext {}

class NoteSchema extends AbstractXMLSchema[NoteSchemaContext] {

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
  list += elementNote

}