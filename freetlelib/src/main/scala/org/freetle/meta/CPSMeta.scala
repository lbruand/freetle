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
package org.freetle.meta

import org.freetle.CPSXMLModel

/**
 * A trait to be added to the model in order to use a meta-processor.
 */

trait CPSMeta[Context] extends CPSXMLModel[Context] {
  class SpaceSkipingMetaProcessor extends MetaProcessor {
    def processTransform(th : TransformBase, instantiate : InstantiateTransform) : ChainedTransformRoot = {
      if (th == !>) {
        th
      } else {
        ((takeSpace)*) ~ instantiate() ~ ((takeSpace)*)
      }
    }
    def processUnaryOperator(th : UnaryOperator, instantiate : InstantiateUnaryOperator, underlying : =>ChainedTransformRoot) : ChainedTransformRoot = {
      instantiate(underlying.metaProcess(this))
    }
    def processBinaryOperator(th : BinaryOperator, instantiate : InstantiateBinaryOperator, left : =>ChainedTransformRoot, right : =>ChainedTransformRoot) : ChainedTransformRoot = {
      instantiate(left.metaProcess(this), right.metaProcess(this))
    }
  }
}