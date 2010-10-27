package org.freetle

@serializable @SerialVersionUID(599494944949L + 1000L)
trait TransformModel[Context] extends FreetleModel[Context] {
	type CFilter =
		XMLResultStream => XMLResultStream
}