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

import scala.util.Random

class WordLadderSpec extends WordSpec with Matchers {

  "WordLadder should find shortest possible transformation between two words given dictionary" in {

    WordLadder.solve("a", "c", Array("a", "b", "c")) shouldBe List("c", "a")
    WordLadder.solve("hit", "cog", Array("hot", "dot", "dog", "lot", "log", "cog")) shouldBe List(
      "cog",
      "dog",
      "dot",
      "hot",
      "hit")
    WordLadder.solve("hit", "cog", Array("hot", "dot", "dog", "lot", "log")) shouldBe List()
    WordLadder.solve("hot", "dog", Array("hot", "dog")) shouldBe List()
    WordLadder.solve("hot", "dog", Array("hot", "cog", "dog", "tot", "hog", "hop", "pot", "dot")) shouldBe List(
      "dog",
      "dot",
      "hot")
    WordLadder.solve("hot", "dog", Array("hot", "cog", "dog", "tot", "hog", "hop", "pot", "dot")) shouldBe List(
      "dog",
      "dot",
      "hot")

    WordLadder.solve(
      "angrb",
      "aYWWb",
      Array(
        "agggb",
        "aggnb",
        "aggYb",
        "aggrb",
        "aggWb",
        "agngb",
        "agnnb",
        "agnYb",
        "agnrb",
        "agnWb",
        "agYgb",
        "agYnb",
        "agYYb",
        "agYrb",
        "agYWb",
        "agrgb",
        "agrnb",
        "agrYb",
        "agrrb",
        "agrWb",
        "agWgb",
        "agWnb",
        "agWYb",
        "agWrb",
        "agWWb",
        "anggb",
        "angnb",
        "angYb",
        "angrb",
        "angWb",
        "anngb",
        "annnb",
        "annYb",
        "annrb",
        "annWb",
        "anYgb",
        "anYnb",
        "anYYb",
        "anYrb",
        "anYWb",
        "anrgb",
        "anrnb",
        "anrYb",
        "anrrb",
        "anrWb",
        "anWgb",
        "anWnb",
        "anWYb",
        "anWrb",
        "anWWb",
        "aYggb",
        "aYgnb",
        "aYgYb",
        "aYgrb",
        "aYgWb",
        "aYngb",
        "aYnnb",
        "aYnYb",
        "aYnrb",
        "aYnWb",
        "aYYgb",
        "aYYnb",
        "aYYYb",
        "aYYrb",
        "aYYWb",
        "aYrgb",
        "aYrnb",
        "aYrYb",
        "aYrrb",
        "aYrWb",
        "aYWgb",
        "aYWnb",
        "aYWYb",
        "aYWrb",
        "aYWWb",
        "arggb",
        "argnb",
        "argYb",
        "argrb",
        "argWb",
        "arngb",
        "arnnb",
        "arnYb",
        "arnrb",
        "arnWb",
        "arYgb",
        "arYnb",
        "arYYb",
        "arYrb",
        "arYWb",
        "arrgb",
        "arrnb",
        "arrYb",
        "arrrb",
        "arrWb",
        "arWgb",
        "arWnb",
        "arWYb",
        "arWrb",
        "arWWb",
        "aWggb",
        "aWgnb",
        "aWgYb",
        "aWgrb",
        "aWgWb",
        "aWngb",
        "aWnnb",
        "aWnYb",
        "aWnrb",
        "aWnWb",
        "aWYgb",
        "aWYnb",
        "aWYYb",
        "aWYrb",
        "aWYWb",
        "aWrgb",
        "aWrnb",
        "aWrYb",
        "aWrrb",
        "aWrWb",
        "aWWgb",
        "aWWnb",
        "aWWYb",
        "aWWrb",
        "aWWWb"
      )
    ) shouldBe List("aYWWb", "aYWrb", "aYgrb", "angrb")

    for (n <- 5 to 24) {
      val a = String.valueOf(Random.alphanumeric.distinct.take(n).toArray)
      val l = for {
        b <- a.substring(0, n - 2)
        c <- a.substring(1, n - 1)
        d <- a.substring(2, n)
        if Random.nextDouble() < 0.1
      } yield "a" + b + c + d + "b"

      val b = l(Random.nextInt(l.length))
      val e = l(Random.nextInt(l.length))
      WordLadder.solve(b, e, l.toArray)
    }
  }

}
