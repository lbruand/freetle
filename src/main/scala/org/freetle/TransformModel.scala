package org.freetle

trait TransformModel[Context] extends FreetleModel[Context] {
	type CFilter =
		XMLResultStream => XMLResultStream
}