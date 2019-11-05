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

import org.scalatest.{Matchers, WordSpec}

class RelatedQuestionsSpec extends WordSpec with Matchers {

  "RelatedQuestions should find the question which minimizes reading time when started from" in {

    RelatedQuestions
      .solve(5, Array(2, 2, 1, 2, 2), Array(Array(0, 1), Array(1, 2), Array(2, 3), Array(3, 4))) shouldBe 2
    RelatedQuestions
      .solve(5, Array(2, 1, 13, 1, 12), Array(Array(3, 0), Array(3, 2), Array(4, 1), Array(3, 1))) shouldBe 3
    RelatedQuestions
      .solve(5, Array(3, 11, 3, 18, 3), Array(Array(3, 1), Array(4, 2), Array(0, 3), Array(4, 1))) shouldBe 4
    RelatedQuestions.solve(1, Array(3), Array()) shouldBe 0
    RelatedQuestions.solve(
      10,
      Array(9, 2, 7, 14, 4, 26, 21, 18, 39, 33),
      Array(
        Array(2, 7),
        Array(0, 9),
        Array(3, 5),
        Array(4, 7),
        Array(0, 2),
        Array(0, 2),
        Array(8, 5),
        Array(3, 6),
        Array(2, 1),
        Array(5, 0))
    ) shouldBe 2
  }

}
