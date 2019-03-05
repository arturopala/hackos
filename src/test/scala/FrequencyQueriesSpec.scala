/*
 * Copyright 2019 arturopala
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

import FrequencyQueries.{freqQuery,parse}
import org.scalatest.{FlatSpec, Matchers}

class FrequencyQueriesSpec extends FlatSpec with Matchers {

  it should "calculate number of occurrences" in {

    freqQuery(Array(Array(1, 1), Array(2, 1), Array(3, 1))) should contain.theSameElementsAs(Array(0))

    freqQuery(parse(
      """8
        |1 5
        |1 6
        |3 2
        |1 10
        |1 10
        |1 6
        |2 5
        |3 2""".stripMargin)) should contain.theSameElementsAs(Array(0,1))

    freqQuery(parse(
      """4
        |3 4
        |2 1003
        |1 16
        |3 1""".stripMargin)) should contain.theSameElementsAs(Array(0,1))

    freqQuery(parse(
      """10
        |1 3
        |2 3
        |3 2
        |1 4
        |1 5
        |1 5
        |1 4
        |3 2
        |2 4
        |3 2""".stripMargin)) should contain.theSameElementsAs(Array(0,1,1))
  }


}
