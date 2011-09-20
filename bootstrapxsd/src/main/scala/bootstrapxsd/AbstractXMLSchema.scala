package bootstrapxsd

import org.freetle.CPSXMLModel
import org.freetle.meta.CPSMeta


class AbstractXMLSchema[Context] extends CPSXMLModel[Context] with CPSMeta[Context] {
  var list = scala.collection.mutable.ArrayBuffer.empty[ChainedTransformRoot]

  abstract class SequenceBaseType extends (()=>ChainedTransformRoot) {
    var list = scala.collection.mutable.ArrayBuffer.empty[ChainedTransformRoot]
    def apply() : ChainedTransformRoot=  list.reduceLeft( (x, y) => new SequenceOperator(x,y) )
  }

  abstract class ChoiceBaseType extends (()=>ChainedTransformRoot) {
    var list = scala.collection.mutable.ArrayBuffer.empty[ChainedTransformRoot]
    def apply() : ChainedTransformRoot=  list.reduceLeft( (x, y) => new ChoiceOperator(x,y) )
  }
}

