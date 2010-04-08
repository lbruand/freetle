package org.freetle

trait TransformModel[Context] extends FreetleModel[Context] {
	type CFilter =
		XMLResultStream => XMLResultStream

	abstract class CFilterBase extends CFilter

	abstract class Operator extends CFilterBase

	abstract case class UnaryOperator(val underlying : CFilterBase) extends
        Operator {
    def clone(underlying : CFilterBase) : UnaryOperator
  }

 	abstract case class BinaryOperator(val left : CFilterBase, val right :CFilterBase) extends
        Operator {
     def clone(left : CFilterBase, right :CFilterBase) : BinaryOperator
  }
}