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
package org.freetle
import org.junit._
import Assert._
import util.QName

case class CPSTranslateModelTstContext()
/**
 * Testing the model translation.
 */
@Test
class CPSTranslateModelTest extends CPSTranslateModel[CPSTranslateModelTstContext] {
  @Test
  def test() = {
    val input = """::header56
::flatfile
::footer55
""".toStream map ( (x : Char) => (Some(Left(x)), false))
    val t = ((const("::") -> drop ~ new ValueTaker(8, new QName("", "header", "")) ~ (const("\n") -> drop))*)
    t(new CFilterIdentity(), new CFilterIdentity())(input, null) foreach (x => print(x))
  }
}