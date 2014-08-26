/*
 * Copyright (C) 06/12/13 Romain Reuillon
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Affero General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

package fr.irstea

import scala.util.Random
import org.apache.commons.math3.random.{ SynchronizedRandomGenerator, RandomAdaptor, RandomGenerator, RandomDataGenerator }
import breeze.linalg._
import breeze.linalg.{ sum => msum }
import breeze.numerics.sqrt
import fr.irstea.scalabc.algorithm.WeightedSimulation

package object scalabc {
  implicit class IteratorExtension[A](i: Iterator[A]) {
    def takeWhileInclusive(p: A => Boolean) = {
      val (a, b) = i.span(p)
      a ++ (if (b.hasNext) Some(b.next) else None)
    }
  }

  implicit def scalaRandomToApacheRandom(rng: Random) =
    new RandomGenerator {
      def setSeed(seed: Long) = rng.setSeed(seed)
      def setSeed(seed: Array[Int]) = ???
      def setSeed(seed: Int) = rng.setSeed(seed)
      def nextBytes(bytes: Array[Byte]) = rng.nextBytes(bytes)
      def nextBoolean() = rng.nextBoolean()
      def nextDouble() = rng.nextDouble()
      def nextLong() = rng.nextLong()
      def nextFloat() = rng.nextFloat()
      def nextGaussian() = rng.nextGaussian()
      def nextInt(n: Int) = rng.nextInt(n)
      def nextInt() = rng.nextInt()
    }

  implicit def apacheRandomToScalaRandom(rng: RandomGenerator) =
    new scala.util.Random(new RandomAdaptor(new SynchronizedRandomGenerator(rng)))

  /**
   * Pick randomly a theta according to their associated weights. The weights are renormalized in the method.
   */
  def pickTheta(thetas: Seq[WeightedSimulation])(implicit rng: Random): WeightedSimulation = {
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

  def covariance(data: DenseMatrix[Double]) = {
    covarianceWeighted(data, DenseVector.fill(data.rows)(1.0 / data.rows.toDouble))
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
    val center = msum(m, Axis._0).toDenseVector
    val x = DenseMatrix.zeros[Double](data.rows, data.cols)
    for (i <- 0 until x.rows) {
      x(i, ::) := data(i, ::) :- center.t
    }
    for (i <- 0 until x.cols) {
      x(::, i) := x(::, i) :* sqrt(weights)
    }
    val w: DenseVector[Double] = weights.map(math.pow(_, 2))
    (x.t * x) / (1 - msum(w))
  }

  /**
   * Draws a Latin Hypercube Sample from a set of uniform distributions.
   * @param nbSamples number of samples
   * @param nbFactors number of factors
   * @param rng the random number generator used
   * @return
   */
  def lhs(nbSamples: Int, nbFactors: Int)(implicit rng: Random): Seq[Seq[Double]] = {
    val rdg = new RandomDataGenerator(rng)
    (0 until nbFactors).map {
      _ =>
        rdg.nextPermutation(nbSamples, nbSamples).map {
          i => (i + rng.nextDouble()) / nbSamples
        }.toSeq
    }.transpose
  }
}
