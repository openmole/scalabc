package fr.irstea.easyabc

import fr.irstea.easyabc.model.prior.PriorFunction
import fr.irstea.easyabc.model.Model
import fr.irstea.easyabc.distance.DistanceFunction
import fr.irstea.easyabc.sampling.ParticleMover
import fr.irstea.easyabc.output.Handler

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

case class Simulation(theta: Seq[Double],
                      summaryStats: Seq[Double],
                      distance: Double)

case class WeightedSimulation(simulation: Simulation, weight: Double)

trait SequentialABC {
  def nextTolerance(): Option[Double]

  def computeWeights(previouslyAccepted: Seq[WeightedSimulation], newAccepted: Seq[Simulation], priors: Seq[PriorFunction[Double]]): Seq[Double]

  def apply(model: Model, priors: Seq[PriorFunction[Double]], nbSimus: Int,
            distanceFunction: DistanceFunction,
            particleMover: ParticleMover,
            outputHandler: Handler)

  /**
   * computes particle weights
   */
  def computeWeightsPrior(particles: Seq[Simulation], priors: Seq[PriorFunction[Double]]): Seq[Double] = {
    for (particle <- particles.map(_.theta)) yield {
      var res = 1.0
      for ((param, prior) <- (particle, priors).zipped) {
        res *= prior.density(param)
      }
      res
    }
  }

  def runSimulations(model: Model, thetas: Seq[Seq[Double]], seeds: Seq[Int]): Seq[Seq[Double]] = {
    (thetas zip seeds).map {
      case (theta, seed) => model.apply(theta, seed)
    }
  }

}
