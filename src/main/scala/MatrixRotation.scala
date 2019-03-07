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

  def matrixRotation(matrix: Array[Array[Int]], rotation: Int): Array[Array[Int]] =
    if (matrix.isEmpty) matrix
    else {
      // compute matrix dimensions
      val rows = matrix.length
      val cols = matrix(0).length
      // compute rings lengths and initialize transformation matrix
      val numberOfRings =
        if (Math.min(rows, cols) % 2 == 0) Math.min(rows, cols) / 2 else (Math.min(rows, cols) + 1) / 2
      val rings = Array.ofDim[Int](numberOfRings)
      val transformation = Array.ofDim[Array[(Int, Int)]](numberOfRings)
      for (ringIndex <- 0 until numberOfRings) {
        val mr = rows - 2 * ringIndex
        val nr = cols - 2 * ringIndex
        val ringLength = if (mr == 1) nr else if (nr == 1) mr else 2 * mr + 2 * nr - 4
        rings(ringIndex) = ringLength
        transformation(ringIndex) = Array.ofDim[(Int, Int)](ringLength)
      }
      // compute transformation matrix
      for (rowIndex <- 0 until rows; colIndex <- 0 until cols) {
        // compute ringIndex and index
        val ringIndex = scala.collection.Seq(rowIndex, colIndex, rows - rowIndex - 1, cols - colIndex - 1).min
        val ringLength = rings(ringIndex)
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
        transformation(ringIndex)(position) = (rowIndex, colIndex)
      }
      val result = Array.ofDim[Int](rows, cols)
      // rotate matrix
      for (ringIndex <- 0 until numberOfRings) {
        val ringLength = rings(ringIndex)
        val offset = rotation % ringLength
        for (position <- 0 until ringLength) {
          val (r1, c1) = transformation(ringIndex)(position)
          val newPosition = (position + offset) % ringLength
          val (r2, c2) = transformation(ringIndex)(newPosition)
          result(r2)(c2) = matrix(r1)(c1)
        }
      }
      result
    }
}
