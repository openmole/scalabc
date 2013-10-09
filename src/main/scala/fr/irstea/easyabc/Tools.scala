package fr.irstea.easyabc

import breeze.linalg.DenseMatrix
import scala.util.Random
import org.apache.commons.math3.random.RandomGenerator

/*
 * Copyright (C) 2013 Nicolas Dumoulin <nicolas.dumoulin@irstea.fr>
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */
object Tools {

  /**
   * Pick randomly a theta according to their associated weights. The weights are renormalized in the method.
   * @param thetas
   * @param rng
   * @return
   */
  def pickTheta(thetas: Seq[WeightedSimulation])(implicit rng: RandomGenerator): WeightedSimulation = {
    val rand = rng.nextDouble()
    val sumWeights = thetas.foldLeft(0.0)((sum, t) => sum + t.weight)
    var sum = 0.0
    for (t <- thetas) {
      sum += t.weight / sumWeights
      if (rand < sum)
        return t
    }
    thetas.last
  }

  def array2DToMatrix(aa: Seq[Seq[Double]]): DenseMatrix[Double] = {
    val xx = DenseMatrix.ones[Double](aa.length, aa(0).length)
    for (r <- 0 until aa.length; c <- 0 until aa(0).length) {
      xx(r, c) = aa(r)(c)
    }
    xx
  }

  def matrixToArray2D(m: DenseMatrix[Double]): Array[Array[Double]] = {
    val aa = Array.fill(m.rows, m.cols)(0.0)
    for (r <- 0 until m.rows; c <- 0 until m.cols) {
      aa(r).update(c, m(r, c))
    }
    aa
  }

}
