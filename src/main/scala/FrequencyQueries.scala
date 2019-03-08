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

object FrequencyQueries {

  /**
    * You are given queries.
    *
    * Each query is of the form two integers described below:
    * - 1 x: Insert x in your data structure.
    * - 2 y: Delete one occurrence of y from your data structure, if present.
    * - 3 z: Check if any integer is present whose frequency is exactly z. If yes, print 1 else 0.
    *
    * Return an integer array consisting of all the outputs of queries of type 3.
    */
  def freqQuery(queries: Array[Array[Int]]): Array[Int] = {

    val counts = scala.collection.mutable.Map[Int, Int]()
    val freqs = scala.collection.mutable.Map[Int, Int]()

    var result: scala.List[Int] = scala.List.empty

    def update(map: scala.collection.mutable.Map[Int, Int], key: Int, fx: Int => Int): Int = {
      val value = map.getOrElse(key, 0)
      map.update(key, Math.max(0, fx(value)))
      value
    }

    queries.foreach(query => {
      val x = query(1)
      query(0) match {
        case 1 =>
          val count = update(counts, x, _ + 1)
          update(freqs, count, _ - 1)
          update(freqs, count + 1, _ + 1)

        case 2 =>
          val count = update(counts, x, _ - 1)
          update(freqs, count, _ - 1)
          update(freqs, count - 1, _ + 1)

        case 3 =>
          val fc = if (freqs.getOrElse(x, 0) > 0) 1 else 0
          result = fc :: result
      }
    })

    result.reverse.toArray[Int]
  }

}
