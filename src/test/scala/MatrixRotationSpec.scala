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

import MatrixRotation.matrixRotation
import org.scalatest.{FlatSpec, Matchers}

class MatrixRotationSpec extends FlatSpec with Matchers {

  it should "rotate the matrix anti-clockwise given number times" in {
    matrixRotation(
      matrix("""
               | 1  2  3  4
               | 5  6  7  8
               | 9 10 11 12
               |13 14 15 16
      """.stripMargin),
      3) shouldBe
      matrix("""
               | 4  8 12 16
               | 3 10  6 15
               | 2 11  7 14
               | 1  5  9 13
      """.stripMargin)

    matrixRotation(
      matrix("""
               | 1  2  3
               | 4  5  6
               | 7  8  9
               |10 11 12
             """.stripMargin),
      2) shouldBe
      matrix("""
               | 3  6  9
               | 2  5 12
               | 1  8 11
               | 4  7 10
             """.stripMargin)

    matrixRotation(
      matrix("""
               | 1  2  3  4
               | 5  6  7  8
               | 9 10 11 12
             """.stripMargin),
      5) shouldBe
      matrix("""
               |12 11 10  9
               | 8  7  6  5
               | 4  3  2  1
             """.stripMargin)

    matrixRotation(
      matrix("""
               | 1  2  3  4  5
               | 6  7  8  9 10
               |11 12 13 14 15
               |16 17 18 19 20
             """.stripMargin),
      2
    ) shouldBe
      matrix("""
               | 3  4  5 10 15
               | 2  9 14 13 20
               | 1  8  7 12 19
               | 6 11 16 17 18
             """.stripMargin)

    matrixRotation(
      matrix("""
               | 1  2  3  4
               | 5  6  7  8
               | 9 10 11 12
               |13 14 15 16
               |17 18 19 20
        """.stripMargin),
      2
    ) shouldBe
      matrix("""
               | 3  4  8 12
               | 2 11 15 16
               | 1  7 14 20
               | 5  6 10 19
               | 9 13 17 18
             """.stripMargin)

    matrixRotation(
      matrix("""
               | 1  2  3  4  5
               | 6  7  8  9 10
               |11 12 13 14 15
               |16 17 18 19 20
               |21 22 23 24 25
               |26 27 28 29 30
             """.stripMargin),
      1
    ) shouldBe
      matrix("""
               | 2  3  4  5 10
               | 1  8  9 14 15
               | 6  7 18 19 20
               |11 12 13 24 25
               |16 17 22 23 30
               |21 26 27 28 29
             """.stripMargin)

    matrixRotation(
      matrix("""
               | 1  2  3
               | 4  5  6
               | 7  8  9
               |10 11 12
               |13 14 15
               |16 17 18
             """.stripMargin),
      2
    ) shouldBe
      matrix("""
               | 3  6  9
               | 2 11 12
               | 1 14 15
               | 4  5 18
               | 7  8 17
               |10 13 16
             """.stripMargin)

    matrixRotation(
      matrix("""
               | 1  2  3  4  5  6  7
               | 8  9 10 11 12 13 14
               |15 16 17 18 19 20 21
               |22 23 24 25 26 27 28
               |29 30 31 32 33 34 35
               |36 37 38 39 40 41 42
             """.stripMargin),
      2
    ) shouldBe
      matrix("""
               | 3  4  5  6  7 14 21
               | 2 11 12 13 20 27 28
               | 1 10 19 26 25 34 35
               | 8  9 18 17 24 33 42
               |15 16 23 30 31 32 41
               |22 29 36 37 38 39 40
             """.stripMargin)

    matrixRotation(
      matrix("""
               | 1  2  3  4  5  6  7  8
               | 9 10 11 12 13 14 15 16
               |17 18 19 20 21 22 23 24
               |25 26 27 28 29 30 31 32
               |33 34 35 36 37 38 39 40
               |41 42 43 44 45 46 47 48
               |49 50 51 52 53 54 55 56
             """.stripMargin),
      2
    ) shouldBe
      matrix("""
               | 3  4  5  6  7  8 16 24
               | 2 12 13 14 15 23 31 32
               | 1 11 21 22 30 38 39 40
               | 9 10 20 28 29 37 47 48
               |17 18 19 27 35 36 46 56
               |25 26 34 42 43 44 45 55
               |33 41 49 50 51 52 53 54
             """.stripMargin)

    //edge cases

    matrixRotation(
      matrix("""
               |
             """.stripMargin),
      3) shouldBe
      matrix("""
               |
             """.stripMargin)

    matrixRotation(
      matrix("""
               |1
             """.stripMargin),
      100) shouldBe
      matrix("""
               |1
             """.stripMargin)

    matrixRotation(
      matrix("""
               |1 2 3
             """.stripMargin),
      1000000) shouldBe
      matrix("""
               |2 3 1
             """.stripMargin)

    val empty = Array.ofDim[Array[Int]](0)
    matrixRotation(empty, 100) shouldBe empty
  }

  "Rotate function" should "rotate the matrix anti-clockwise" in {
    val rotation = MatrixRotation.Rotate(4, 5)
    val m1 = matrix("""
                      | 1  2  3  4  5
                      | 6  7  8  9 10
                      |11 12 13 14 15
                      |16 17 18 19 20
                       """.stripMargin)
    val m2 = matrix("""
                      |3  4  5 10 15
                      |2  9 14 13 20
                      |1  8  7 12 19
                      |6 11 16 17 18
             """.stripMargin)
    rotation(2)(m1) shouldBe m2
    m2 should not be theSameInstanceAs(m1)
  }

  "Rotate function" should "rotate the matrix anti-clockwise in place (mutate)" in {
    val rotation = MatrixRotation.Rotate(4, 5)
    val m1 = matrix("""
                      | 1  2  3  4  5
                      | 6  7  8  9 10
                      |11 12 13 14 15
                      |16 17 18 19 20
                    """.stripMargin)
    val m2 = rotation(2, mutate = true)(m1)
    m2 shouldBe matrix("""
                         |3  4  5 10 15
                         |2  9 14 13 20
                         |1  8  7 12 19
                         |6 11 16 17 18
                       """.stripMargin)
    m2 shouldBe theSameInstanceAs(m1)
  }

  "Rotate function" should "rotate the matrix clockwise" in {
    val rotation = MatrixRotation.Rotate(4, 5)
    val m1 = matrix("""
                      | 1  2  3  4  5
                      | 6  7  8  9 10
                      |11 12 13 14 15
                      |16 17 18 19 20
                    """.stripMargin)
    val m2 = rotation(-2)(m1)
    m2 shouldBe matrix("""
                         |11  6  1  2  3
                         |16 13 12  7  4
                         |17 14  9  8  5
                         |18 19 20 15 10
                       """.stripMargin)
    m2 should not be theSameInstanceAs(m1)
  }

  "Rotate function" should "rotate the matrix clockwise in place (mutate)" in {
    val rotation = MatrixRotation.Rotate(4, 5)
    val m1 = matrix("""
                      | 1  2  3  4  5
                      | 6  7  8  9 10
                      |11 12 13 14 15
                      |16 17 18 19 20
                    """.stripMargin)
    val m2 = rotation(-2, mutate = true)(m1)
    m2 shouldBe matrix("""
                         |11  6  1  2  3
                         |16 13 12  7  4
                         |17 14  9  8  5
                         |18 19 20 15 10
             """.stripMargin)
    m2 shouldBe theSameInstanceAs(m1)
  }

  "Rotate function" should "throw an exception when shape not match" in {
    val rotation = MatrixRotation.Rotate(4, 5)
    an[Exception] shouldBe thrownBy(rotation(1)(matrix(5, 4)))
    an[Exception] shouldBe thrownBy(rotation(1)(matrix(4, 4)))
    an[Exception] shouldBe thrownBy(rotation(1)(matrix(5, 5)))
  }

  "Rotate function" should "rotate the large matrix" in {
    val rotation = MatrixRotation.Rotate(100, 100)
    val m1 = matrix(100, 100)
    val m2 = rotation(37, mutate = true)(m1)
    m1.map(_.sum).sum shouldBe m2.map(_.sum).sum
  }

  def matrix(s: String): Array[Array[Int]] = {
    val matrix = s.lines.toArray
      .drop(1)
      .init
      .map(
        _.split("\\s+")
          .filterNot(_.isEmpty)
          .map(_.toInt)
          .toArray)
    matrix
  }

  def matrix(m: Int, n: Int): Array[Array[Int]] = {
    val matrix = Array.ofDim[Int](m, n)
    for (i <- 0 until m) {
      for (j <- 0 until n) {
        matrix(i)(j) = i * n + j + 1
      }
    }
    matrix
  }

  def show(array: Array[Array[Int]]): String = {
    val m = array.map(_.map(c => f"$c%2d").mkString(" ")).mkString("\n")
    println("-" * 20)
    println(m)
    m
  }

}
