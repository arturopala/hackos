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

object MatrixRotation {

  /**
    * You are given a 2D matrix of dimension  and a positive integer.
    * You have to rotate the matrix r times and print the resultant matrix.
    * Rotation should be in anti-clockwise direction if positive, otherwise clockwise.
    * It is NOT guaranteed that the minimum of m and n will be even.
    *
    * Example:
    * - rotation by 3
    *
    *    1  2  3  4        4  8 12 16
    *    5  6  7  8   ->   3 10  6 15
    *    9 10 11 12   ->   2 11  7 14
    *   13 14 15 16        1  5  9 13
    *
    */
  def matrixRotation(matrix: Array[Array[Int]], r: Int): Array[Array[Int]] =
    if (matrix.isEmpty) matrix
    else {
      // compute matrix dimensions
      val rows = matrix.length
      val cols = matrix(0).length
      val rotate = Rotate(rows, cols)
      // rotate
      rotate(r)(matrix)
    }

  type MatrixTransformation = Array[Array[Int]] => Array[Array[Int]]

  /** Function to rotate matrix of the given shape  in the linear time */
  case class Rotate(rows: Int, cols: Int) {
    // compute rings lengths and initialize transformation table
    val numberOfRings =
      if (Math.min(rows, cols) % 2 == 0) Math.min(rows, cols) / 2 else (Math.min(rows, cols) + 1) / 2
    val rings = Array.ofDim[Array[(Int, Int)]](numberOfRings)
    for (ringIndex <- 0 until numberOfRings) {
      val mr = rows - 2 * ringIndex
      val nr = cols - 2 * ringIndex
      val ringLength = if (mr == 1) nr else if (nr == 1) mr else 2 * mr + 2 * nr - 4
      rings(ringIndex) = Array.ofDim[(Int, Int)](ringLength)
    }
    // compute transformation table
    for (rowIndex <- 0 until rows; colIndex <- 0 until cols) {
      // compute ringIndex and index
      val ringIndex = scala.collection.Seq(rowIndex, colIndex, rows - rowIndex - 1, cols - colIndex - 1).min
      val ringLength = rings(ringIndex).length
      val lm = rows - 2 * ringIndex
      val ln = cols - 2 * ringIndex
      val li = rowIndex - ringIndex
      val lj = colIndex - ringIndex
      val position =
        if (lj == 0) li
        else if (li == 0) ringLength - lj
        else if (li == lm - 1) lm + lj - 1
        else ringLength - ln - li + 1
      // remember matrix location for the ring position
      rings(ringIndex)(ringLength - position - 1) = (rowIndex, colIndex)
    }

    def apply(steps: Int, mutate: Boolean = false): MatrixTransformation = { matrix =>
      require(
        matrix.length == rows && matrix(0).length == cols,
        s"Matrix to rotate must have the expected shape $rows x $cols but was ${matrix.length} x ${matrix(0).length}"
      )
      if (steps == 0) matrix
      else {
        val result = if (mutate) matrix else Array.ofDim[Int](rows, cols)
        // rotate matrix
        for (ringIndex <- 0 until numberOfRings) {
          val ringLength = rings(ringIndex).length
          val offset = Math.abs(steps) % ringLength
          val ring = if (steps < 0) rings(ringIndex).reverse else rings(ringIndex)
          val cache = Array.ofDim[Int](Math.abs(offset))
          for (i <- 0 until offset) {
            val (r, c) = ring(i)
            cache(i) = matrix(r)(c)
          }
          for (i <- offset until ringLength) {
            val (r1, c1) = ring(i)
            val (r2, c2) = ring(i - Math.abs(offset))
            result(r2)(c2) = matrix(r1)(c1)
          }
          for (i <- 0 until offset) {
            val (r2, c2) = ring(ringLength - offset + i)
            result(r2)(c2) = cache(i)
          }
        }
        result
      }
    }
  }
}
