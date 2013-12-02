package fr.irstea.easyabc

import fr.irstea.easyabc.model.prior.PriorFunction
import fr.irstea.easyabc.model.Model
import fr.irstea.easyabc.distance.DistanceFunction
import fr.irstea.easyabc.sampling.{JabotMoving, ParticleMover}

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

case class State(iteration: Int, nbSimulatedThisStep: Int, nbSimulatedTotal: Int, tolerance: Double, accepted: Option[Seq[WeightedSimulation]], varSummaryStats: Option[Seq[Double]])

trait SequentialABC {

  def tolerancesIt(): Iterator[Double]

  def computeWeights(previouslyAccepted: Seq[WeightedSimulation], newAccepted: Seq[Simulation], priors: Seq[PriorFunction[Double]]): Seq[Double]

  def step(model: Model, priors: Seq[PriorFunction[Double]], nbSimus: Int, tolerance: Double,
           previousState: State,
           distanceFunction: DistanceFunction,
           particleMover: ParticleMover): State

  def apply(model: Model, priors: Seq[PriorFunction[Double]], nbSimus: Int,
            distanceFunction: DistanceFunction,
            particleMover: ParticleMover = new JabotMoving()): Iterator[State] = {
    var previousState = new State(0, 0, 0, 0, None, None)
    tolerancesIt().map {
      tol =>
        previousState = step(model, priors, nbSimus, tol, previousState, distanceFunction, particleMover)
        previousState
    }
    // prettier but the last tolerance is skipped
    // val tolerances = tolerancesIt()
    //Iterator.iterate(new State(0, 0, 0, 0, None, None))(step(model, priors, nbSimus, tolerances.next, _, distanceFunction, particleMover)) takeWhile (_ => tolerances.hasNext)
  }

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
