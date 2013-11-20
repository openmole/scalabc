package fr.irstea.easyabc

import breeze.linalg._
import breeze.linalg.{sum => msum}
import breeze.numerics.sqrt
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

  /**
   * Computes the estimated weighted covariance matrix.
   * The code has been written from the implementation of the function cov.wt in R 2.15.2.
   * The weights are not renormalized.
   */
  def covarianceWeighted(data: DenseMatrix[Double], weights: DenseVector[Double]) = {
    val m = data.copy
    for (i <- 0 until data.cols) {
      m(::, i) := m(::, i) :* weights
    }
    val center = msum(m, Axis._0)
    val x = DenseMatrix.zeros[Double](data.rows, data.cols)
    for (i <- 0 until x.rows) {
      x(i, ::) := data(i, ::) :- center
    }
    for (i <- 0 until x.cols) {
      x(::, i) := x(::, i) :* sqrt(weights)
    }
    val square = breeze.generic.UFunc {
      (d: Double) => math.pow(d, 2)
    }
    val w: DenseVector[Double] = square(weights)
    (x.t * x) / (1 - w.sum)
  }
}
