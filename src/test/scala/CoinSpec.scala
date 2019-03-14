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

import Coins._

class CoinSpec extends WordSpec with Matchers {

  "Coin" when {
    "sum" should {
      "return 0 if empty list of coins" in {
        Coins.sum(Nil) == 0
      }

      "return coin value if only single coin in the list" in {
        sum(List(C1)) == 1
        sum(List(C2)) == 2
        sum(List(C5)) == 5
        sum(List(C10)) == 10
        sum(List(C20)) == 20
        sum(List(C50)) == 50
        sum(List(E1)) == 100
        sum(List(E2)) == 200
      }

      "return sum of all coin values if list not empty" in {
        sum(List(C1, C2)) == 3
        sum(List(C2, C5)) == 7
        sum(List(C5, C1, C2)) == 8
        sum(List(C5, E2, C1, C2, E1)) == 308
      }
    }

    "changes" should {

      "compute possible changes of 0" in {
        changes(0) shouldBe Nil
      }

      "compute possible changes of 1" in {
        changes(1) shouldBe List(List(C1))
      }

      "compute possible changes of 2" in {
        changes(2) shouldBe List(List(C2), List(C1, C1))
      }

      "compute possible changes of 3" in {
        changes(3) shouldBe List(List(C2, C1), List(C1, C1, C1))
      }

      "compute possible changes of 10" in {
        changes(10) shouldBe List(
          List(C10),
          List(C5, C5),
          List(C5, C2, C2, C1),
          List(C5, C2, C1, C1, C1),
          List(C2, C2, C2, C2, C2),
          List(C5, C1, C1, C1, C1, C1),
          List(C2, C2, C2, C2, C1, C1),
          List(C2, C2, C2, C1, C1, C1, C1),
          List(C2, C2, C1, C1, C1, C1, C1, C1),
          List(C2, C1, C1, C1, C1, C1, C1, C1, C1),
          List(C1, C1, C1, C1, C1, C1, C1, C1, C1, C1)
        )
      }

      "compute possible changes of 13 having size no greater than 7" in {
        changes(13, maxNumberOfCoins = 7) shouldBe List(
          List(C10, C2, C1),
          List(C10, C1, C1, C1),
          List(C5, C5, C2, C1),
          List(C5, C5, C1, C1, C1),
          List(C5, C2, C2, C2, C2),
          List(C5, C2, C2, C2, C1, C1),
          List(C5, C2, C2, C1, C1, C1, C1),
          List(C2, C2, C2, C2, C2, C2, C1)
        )
      }

      "compute possible changes of 13 having size no greater than 0" in {
        changes(13, maxNumberOfCoins = 0) shouldBe Nil
      }

      "compute possible changes of 10 given C5 and C2 only" in {
        changes(10, denominations = Set(C2, C5)) shouldBe List(List(C5, C5), List(C2, C2, C2, C2, C2))

      }

      "compute possible changes of 10 given no coins" in {
        changes(10, denominations = Set()) shouldBe Nil

      }
    }

    "changesStream" should {
      "return the smallest size change at the head" in {
        changesStream(2).head shouldBe List(C2)
        changesStream(5).head shouldBe List(C5)
        changesStream(10).head shouldBe List(C10)
        changesStream(8).head shouldBe List(C1, C2, C5)
        changesStream(9).head shouldBe List(C2, C2, C5)
        changesStream(13).head shouldBe List(C1, C2, C10)
      }

      "return the next change ordered by the size" in {
        changesStream(2)(1) shouldBe List(C1, C1)

        changesStream(5)(1) shouldBe List(C1, C2, C2)
        changesStream(5)(2) shouldBe List(C1, C1, C1, C2)
        changesStream(5)(3) shouldBe List(C1, C1, C1, C1, C1)

        changesStream(10)(1) shouldBe List(C5, C5)
        changesStream(10)(2) shouldBe List(C1, C2, C2, C5)

        changesStream(8)(1) shouldBe List(C1, C1, C1, C5)
        changesStream(8)(2) shouldBe List(C1, C1, C1, C1, C2, C2)

        changesStream(9)(1) shouldBe List(C1, C1, C2, C5)
        changesStream(13)(1) shouldBe List(C1, C1, C1, C10)
      }
    }

  }

}
