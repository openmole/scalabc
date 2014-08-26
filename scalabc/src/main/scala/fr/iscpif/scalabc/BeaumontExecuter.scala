/*
 * Copyright (C) 16/01/14 Romain Reuillon
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

package fr.iscpif.scalabc

import fr.iscpif.scalabc.executer.SequentialExecuter
import fr.iscpif.scalabc.algorithm.{ WeightedSimulation, Simulation, Beaumont }
import scala.util.Random
import scala.collection.mutable.ListBuffer
import org.apache.commons.math3.stat.descriptive.DescriptiveStatistics

trait BeaumontExecuter <: Beaumont with SequentialExecuter {
  override def step(previousState: STATE)(implicit rng: Random): BeaumontState = {
    var varSummaryStats: Seq[Double] = Nil
    var nbSimulated = 0
    val newAccepted = ListBuffer.empty[Simulation]
    while (newAccepted.size < simulations) {
      // we need this amount of accepted simulations to reach nbSimus
      val remainingSimusForThisStep = simulations - newAccepted.size
      val thetas = sample(previousState, remainingSimusForThisStep)
      // running simulations
      val summaryStats = runSimulations(thetas)
      // determination of the normalization constants in each dimension associated to each summary statistic, this normalization will not change during all the algorithm
      varSummaryStats = previousState.varSummaryStats.getOrElse(
        for (col <- 0 until summaryStatsTarget.length) yield math.min(1.0, 1 / new DescriptiveStatistics(summaryStats.map(_(col)).toArray).getVariance)
      )
      // selecting the tolerable simulations
      for (s <- selectSimulation(thetas, summaryStats, varSummaryStats, previousState.tolerance)) {
        newAccepted += s
      }
      nbSimulated += thetas.length
    } // until we get nbSimus simulations below the tolerance threshold
    // compute weights
    val weights: Seq[Double] =
      previousState.accepted match {
        case None =>
          // initial step
          Array.fill(simulations)(1 / simulations.toDouble)
        case Some(accepted) =>
          // following steps
          computeWeights(accepted, newAccepted)
      }
    val sumWeights = weights.sum
    // go to the next tolerance
    BeaumontState(
      previousState.iteration + 1,
      previousState.toleranceIndex + 1,
      Some(for ((s, w) <- newAccepted zip weights) yield WeightedSimulation(s, w / sumWeights)),
      Some(varSummaryStats),
      nbSimulated,
      previousState.evaluations + nbSimulated
    )
  }
}
