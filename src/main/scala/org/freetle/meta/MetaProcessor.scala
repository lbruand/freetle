package org.freetle.meta

import org.freetle.transform.{TakeSpace, UnaryOperator, CFilterBase}

/**
 * Created by IntelliJ IDEA.
 * User: luke
 * Date: 26 mars 2010
 * Time: 21:19:34
 * To change this template use File | Settings | File Templates.
 */

abstract class MetaProcessor extends (CFilterBase => CFilterBase)

class RecursiveMetaProcessor extends MetaProcessor {
  def apply(in: CFilterBase) : CFilterBase = {
    if (in.isInstanceOf[UnaryOperator]) {
      new TakeSpace()
    } else {
      null
    }
    //in match {
      //case UnaryOperator(underlying : CFilterBase)
       // => null
    //}
  }
}