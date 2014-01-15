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

package fr.irstea.scalabc.sampling

import fr.irstea.scalabc._
import breeze.linalg._
import fr.irstea.scalabc.WeightedSimulation
import org.apache.commons.math3.distribution.MultivariateNormalDistribution
import scala.util.Random

trait LenormandMover extends ParticleMover {

  /**
   * implementation based on the the implementation of Lenormand
   * TODO seems to not working
   */
  def move(simulations: Seq[WeightedSimulation])(implicit rng: Random): Seq[Double] = {
    val weightsVector = DenseVector((for (s <- simulations) yield s.weight).toArray)
    val thetas = simulations.map(_.simulation.theta)
    val M = array2DToMatrix(simulations.map(_.simulation.theta))
    val V = diag(weightsVector)
    val Wt = DenseMatrix.eye[Double](simulations.length)
    Wt(0, ::) := weightsVector
    val sumwt2 = weightsVector.toArray.foldLeft(0.0)(_ + math.pow(_, 2))
    val C = (M.t * V * M - (Wt * M).t * Wt * M) * (1 / (1 - sumwt2))
    val Cb = Array.fill(C.rows, C.rows)(0.0)
    for (r <- 0 until C.rows) {
      Cb(r).update(r, C(r, r))
    }

    val dist = new MultivariateNormalDistribution(rng, pickTheta(simulations).simulation.theta.toArray, Cb)
    // TODO check if in bounds?
    dist.sample()
  }

}
