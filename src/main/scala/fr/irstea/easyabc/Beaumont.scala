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

package fr.irstea.easyabc

import fr.irstea.easyabc.model.Model
import fr.irstea.easyabc.prior.PriorFunction
import fr.irstea.easyabc.distance.Distance
import scala.collection.mutable.ListBuffer
import org.apache.commons.math3.random.RandomGenerator
import scala.Some
import fr.irstea.easyabc.sampling.ParticleMover
import breeze.stats.DescriptiveStats
import breeze.linalg.{ pow => bpow, DenseVector }
import breeze.numerics.{ exp => bexp }
import org.apache.commons.math3.stat.descriptive.DescriptiveStatistics
import SequentialABC._
import scala.util.Random

trait Beaumont extends SequentialABC {

  case class BeaumontState(
      iteration: Int,
      toleranceIndex: Int,
      accepted: Option[Seq[WeightedSimulation]],
      varSummaryStats: Option[Seq[Double]],
      evaluationsForStep: Int,
      evaluations: Int) extends State {
    def tolerance = tolerances(toleranceIndex)
  }

  type STATE = BeaumontState

  def tolerances: Seq[Double]

  def initialState = BeaumontState(0, 0, None, None, 0, 0)
  def finished(s: STATE): Boolean = s.toleranceIndex >= tolerances.size

  //override def tolerancesIt(): Iterator[Double] = tolerances.iterator

  /**
   * computes particle weights with unidimensional jumps
   */
  def computeWeights(previouslyAccepted: Seq[WeightedSimulation], newAccepted: Seq[Simulation]): Seq[Double] = {
    val nbParam = previouslyAccepted(0).simulation.theta.length
    val nbParticle = previouslyAccepted.length
    val nbNewParticle = newAccepted.length
    val var_array = (0 until nbParam).map(col => 4 * DescriptiveStats.variance(previouslyAccepted.map(_.simulation.theta(col))))
    val multi = var_array.foldLeft(math.pow(1 / math.sqrt(2 * math.Pi), nbParam))((s, t) => s * 1 / math.sqrt(t / 2))
    var weights = DenseVector.zeros[Double](nbNewParticle)
    for (i <- 0 until nbParticle) {
      var tab_temp = DenseVector.fill(nbNewParticle)(previouslyAccepted(i).weight * multi)
      for (k <- 0 until nbParam) {
        val theta_i_k: Double = previouslyAccepted(i).simulation.theta(k)
        val tmp: DenseVector[Double] = DenseVector(newAccepted.map(_.theta(k) - theta_i_k).toArray)
        tab_temp = tab_temp :* bexp(-(tmp :* tmp) / var_array(k))
      }
      weights = weights + tab_temp
    }
    val tab_weight_prior = computeWeightsPrior(newAccepted, priors)
    (tab_weight_prior zip weights.data).map {
      case (twp: Double, w: Double) => {
        twp / (w * weights.sum)
      }
    }
  }

  def selectSimulation(thetas: Seq[Seq[Double]], summaryStats: Seq[Seq[Double]], var_summaryStats: Seq[Double], tolerance: Double): Seq[Simulation] = {
    val simus: Seq[Simulation] = for ((theta, summaryStat) <- thetas zip summaryStats) yield {
      new Simulation(theta, summaryStat, distance = distance(summaryStat, var_summaryStats))
    }
    //simus.map(s => println(s.theta + " = " + s.summaryStats + " -> " + s.distance))
    simus.filter(_.distance < tolerance)
  }

  def sample(
    previousState: BeaumontState,
    nbSimus: Int)(implicit rng: Random): Seq[Seq[Double]] =
    // sampling thetas
    (0 until nbSimus).map(_ =>
      previousState.accepted match {
        case None =>
          for (p <- priors) yield p.value
        case Some(accepted) => move(accepted)
      })

  override def step(
    model: Model)(previousState: STATE)(implicit rng: Random): BeaumontState = {
    var varSummaryStats: Seq[Double] = Nil
    var nbSimulated = 0
    val newAccepted = ListBuffer.empty[Simulation]
    while (newAccepted.size < simulations) {
      // we need this amount of accepted simulations to reach nbSimus
      val remainingSimusForThisStep = simulations - newAccepted.size
      val thetas = sample(previousState, remainingSimusForThisStep)
      // running simulations
      val summaryStats = runSimulations(model, thetas)
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
