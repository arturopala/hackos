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

import scala.annotation.tailrec
sealed class Coin(val value: Int)

object Coins {

  case object C1 extends Coin(1)
  case object C2 extends Coin(2)
  case object C5 extends Coin(5)
  case object C10 extends Coin(10)
  case object C20 extends Coin(20)
  case object C50 extends Coin(50)

  case object E1 extends Coin(100)
  case object E2 extends Coin(200)

  implicit val ord: Ordering[Coin] = Ordering.by(_.value)

  val coins: Set[Coin] = Set(C1, C2, C5, C10, C20, C50, E1, E2)

  def sum(coins: List[Coin]): Int = coins.foldLeft(0)((s, c) => s + c.value)

  /**
    * Returns a list of all possible changes of the amount given denominations and maximum size of a change.
    * The first change has the smallest size.
    * */
  def changes(amount: Int, maxNumberOfCoins: Int = 10, denominations: Set[Coin] = Coins.coins): List[List[Coin]] =
    if (amount <= 0 || maxNumberOfCoins <= 0 || denominations.isEmpty) Nil
    else {

      @tailrec
      def compute(variants: List[List[Coin]]): List[List[Coin]] = {
        val denominationsSorted = denominations.toList.sorted
        val variants2 = variants.flatMap(
          coins =>
            if (sum(coins) == amount) List(coins)
            else
              denominationsSorted
                .map(c =>
                  coins match {
                    case head :: _ if c.value >= head.value => c :: coins
                    case Nil                                => c :: Nil
                    case _                                  => Nil
                })
                .filterNot(v => v.isEmpty || v.length > maxNumberOfCoins || sum(v) > amount))
        if (variants2 == variants) variants else compute(variants2)
      }

      compute(List(Nil)).sortBy(_.length)
    }

  /**
    * Returns a stream of all possible changes of the amount given coin types and maximum size of a change.
    * The first change has the smallest size.
    * */
  def changesStream(
    amount: Int,
    maxNumberOfCoins: Int = 10,
    availableDenominations: Set[Coin] = Coins.coins): Stream[List[Coin]] =
    if (amount <= 0 || maxNumberOfCoins <= 0 || availableDenominations.isEmpty) Stream.empty
    else {

      val (denominations, smallest) = {
        val ds = availableDenominations.toList.sorted
        (ds.reverse, ds.head)
      }

      def computeHead(amount: Int, available: List[Coin]): List[Coin] =
        if (amount == 0) Nil
        else
          available match {
            case Nil => Nil
            case coin :: remainingCoins => {
              val quantity = amount / coin.value
              val remainder = amount - quantity * coin.value
              computeHead(remainder, remainingCoins) ::: List.fill(quantity)(coin)
            }
          }

      def computeNext(change: List[Coin]): List[Coin] =
        change match {
          case Nil                      => Nil
          case c :: cs if c != smallest => computeHead(c.value, denominations.filter(_.value < c.value)) ::: cs
          case c :: cs                  => c :: computeNext(cs)
        }

      val head = computeHead(amount, denominations)

      def tail(head: List[Coin]): Stream[List[Coin]] = head match {
        case Nil => Stream.empty
        case css =>
          val next = computeNext(css)
          if (next == head) Stream.empty else next #:: tail(next)
      }

      if (sum(head) < amount) Stream.empty else head #:: tail(head)
    }
}
