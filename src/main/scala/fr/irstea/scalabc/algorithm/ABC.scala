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

package fr.irstea.scalabc.algorithm

import fr.irstea.scalabc.prior.PriorFunction
import fr.irstea.scalabc.model.Model
import fr.irstea.scalabc.distance.Distance
import fr.irstea.scalabc.sampling.{ JabotMover, ParticleMover }
import scala.util.Random

case class Simulation(theta: Seq[Double],
  summaryStats: Seq[Double],
  distance: Double)

case class WeightedSimulation(simulation: Simulation, weight: Double)

trait State {
  def iteration: Int
  def accepted: Option[Seq[WeightedSimulation]]
  def varSummaryStats: Option[Seq[Double]]
  def tolerance: Double
  def evaluationsForStep: Int
  def evaluations: Int
}

object ABC {
  /**
   * computes particle weights
   */
  def computeWeightsPrior(
    particles: Seq[Simulation],
    priors: Seq[PriorFunction]): Seq[Double] = {
    for (particle <- particles.map(_.theta)) yield {
      var res = 1.0
      for ((param, prior) <- (particle, priors).zipped) {
        res *= prior.density(param)
      }
      res
    }
  }

}

trait ABC <: ParticleMover
    with Distance {

  type STATE <: State

  def initialState: STATE
  def finished(s: STATE): Boolean
  def simulations: Int
  def summaryStatsTarget: Seq[Double]
  def priors: Seq[PriorFunction]

}
