package org.freetle.meta

import org.freetle.transform._

/**
 * Created by IntelliJ IDEA.
 * User: luke
 * Date: 26 mars 2010
 * Time: 21:19:34
 * To change this template use File | Settings | File Templates.
 */

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