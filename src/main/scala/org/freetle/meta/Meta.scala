package org.freetle.meta


import org.freetle.Transform

/**
 * Created by IntelliJ IDEA.
 * User: luke
 * Date: 26 mars 2010
 * Time: 21:19:34
 * To change this template use File | Settings | File Templates.
 */
trait Meta[Context] extends Transform[Context] {
  abstract class MetaProcessor extends (CFilterBase => CFilterBase)

  abstract class RecursiveMetaProcessor extends MetaProcessor {

    def map(in: BaseTransform) : CFilterBase

    def apply(in: CFilterBase) : CFilterBase = {

      in match {        
        case unary @ UnaryOperator(underlying) => unary.clone(this(underlying))
        case binary @ BinaryOperator(left, right) => binary.clone(this(left), this(right))
        case transfo : BaseTransform => map(transfo)
      }
    }
  }

  class SpaceSkipingMetaProcessor extends RecursiveMetaProcessor {
    def map(in: BaseTransform) = {
      if (!in.isInstanceOf[DropFilter]) {
        val takeSpc = new WhileNoResultOperator(new TakeSpace())
        new SequenceOperator(takeSpc, new SequenceOperator(in, takeSpc) )
      } else {
        in
      }
    }
  }
}