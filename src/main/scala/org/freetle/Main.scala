 /*
  * Copyright 2010 Lucas Bruand
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
package org.freetle


import scala._
import scala.Stream
import util.{StreamSource, EvElemStart}

class MainContext

object Main extends Transform[MainContext] {

   


	val el = Stream.concat("<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n<body>\n".toList.toStream,
                         Stream.concat(Stream.make(10000000, "<msg/>\n".toList.toStream)),
                         "</body>\n".toList.toStream).elements
    val src = StreamSource.fromIterator(el)
    
    def main(args: Array[String]) {
      //println(el.foldLeft(0)( (x,y) => x ^ y.hashCode()))
     
      
      /*val id =  new IdTransform()      
      println(id(Stream.fromIterator( new XMLEventStream(src) map (Tail(_)) ) ).foldLeft(0)( (x,y) => x ^ y.hashCode()))*/
      //idResult.foreach(x => println(x))
      val depthTest = 100
    	val i = Stream.make(depthTest, new EvElemStart(new javax.xml.namespace.QName("http://namespace.com/", "p", "message" ),  null)) map (Tail(_, null))
		val t = new RepeatUntilNoResultOperator(new TakeStartElement("message"))
		val r = t(i)
        println(r)
        println ( r.filter(x => x match {
		  case Result(_, _) => true
		  case _ => false
		}).length)
    }
}
                   
