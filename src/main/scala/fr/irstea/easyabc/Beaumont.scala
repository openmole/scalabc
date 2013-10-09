package fr.irstea.easyabc

import fr.irstea.easyabc.model.Model
import fr.irstea.easyabc.model.prior.PriorFunction
import fr.irstea.easyabc.distance.{DefaultDistance, DistanceFunction}
import scala.collection.mutable.ArrayBuffer
import breeze.linalg._
import fr.irstea.easyabc.Tools._
import org.apache.commons.math3.distribution.MultivariateNormalDistribution
import org.apache.commons.math3.random.RandomGenerator
import scala.Some

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

class Beaumont(val tolerances: Seq[Double], val summaryStatsTarget: Seq[Double])(implicit rng: RandomGenerator) extends SequentialABC {


  var currentStep = 0

  def nextTolerance(): Option[Double] = {
    if (currentStep > tolerances.length) {
      None
    } else {
      Some(tolerances(currentStep))
    }
  }

  def gaussianWalk(simulations: Seq[WeightedSimulation]): Seq[Double] = {
    val weightsVector = DenseVector((for (s <- simulations) yield s.weight).toArray)
    val thetas = simulations.map(_.simulation.theta)
    val M = array2DToMatrix(simulations.map(_.simulation.theta))
    val V = diag(weightsVector)
    val Wt = DenseMatrix.eye[Double](simulations.length)
    Wt(0, ::) := weightsVector
    val sumwt2 = weightsVector.toArray.foldLeft(0.0)(_ + math.pow(_, 2))
    val C = (M.t * V * M - Wt * M.t * Wt * M) * (1 / (1 - sumwt2))
    val Cb = Array.fill(C.rows, C.rows)(0.0)
    for (r <- 0 until C.rows) {
      Cb(r).update(r, C(r, r))
    }
    val dist = new MultivariateNormalDistribution(rng, pickTheta(simulations).simulation.theta.toArray, Cb)
    // TODO check if in bounds?
    dist.sample()
  }

  def apply(model: Model, useSeed: Boolean=false, prior: Seq[PriorFunction[Double]], nbSimus: Int, distanceFunction: DistanceFunction = new DefaultDistance(summaryStatsTarget)) = {
    currentStep = 0
    var tolerance = nextTolerance()
    var accepted: Seq[WeightedSimulation] = Nil
    while (tolerance != None) {
      val newAccepted = ArrayBuffer.empty[Simulation]
      var currentSeed = 0
      // TODO run several (nbSimus - newAccepted.size) simulations at a time, for further parallelisation
      while (newAccepted.size < nbSimus) {
        var theta: Seq[Double] = Nil
        if (currentStep == 0) {
          theta = for (p <- prior) yield p.value()
        } else {
          theta = gaussianWalk(accepted)
        }
        val summaryStats = model.apply(theta, if (useSeed) Some(currentSeed) else None)
        val distance = distanceFunction.distance(summaryStats)
        if (distance <= tolerance.get) {
          newAccepted += new Simulation(theta, summaryStats, distance)
        }
        currentSeed += 1
      }
      // compute weights
      val distanceMax: Double = newAccepted.foldLeft(0.0)(_ max _.distance)
      // TODO fonction computeWeights
      // TODO prendre une meilleure formule de calcul de poids
      var weights: Seq[Double] = Nil
      if (currentStep == 0) {
        weights = Array.fill(nbSimus)(1 / nbSimus.toDouble)
      } else {
        weights = for (s <- newAccepted) yield 1 - math.pow(s.distance / distanceMax, 2)
      }
      val sumWeights = weights.sum
      accepted = for ((s, w) <- newAccepted zip weights) yield WeightedSimulation(s, w / sumWeights)
      // go to the next tolerance
      currentStep += 1
      tolerance = nextTolerance()
      // TODO écrire les fichiers
    }

  }
}
