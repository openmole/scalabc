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

package fr.irstea.scalabc

import fr.irstea.scalabc.prior.{ Uniform, PriorFunction }
import org.apache.commons.math3.stat.descriptive.DescriptiveStatistics
import breeze.linalg._
import util.Random

case class LenormanState(
  iteration: Int,
  tolerance: Double,
  accepted: Option[Seq[WeightedSimulation]],
  varSummaryStats: Option[Seq[Double]],
  proportionOfAccepted: Double,
  evaluationsForStep: Int,
  evaluations: Int) extends State

import SequentialABC._

trait Lenormand <: SequentialABC {

  type STATE = LenormanState

  def alpha: Double = 0.5
  def pAccMin: Double = 0.05
  def priors: Seq[Uniform]

  def initialState = LenormanState(
    iteration = 0,
    tolerance = 0.0,
    None,
    None,
    pAccMin + 1,
    0,
    0)

  def finished(s: STATE) = s.proportionOfAccepted <= pAccMin

  def computeWeights(previouslyAccepted: Seq[WeightedSimulation], newAccepted: Seq[Simulation]): Seq[Double] = {
    val nbParam = previouslyAccepted(0).simulation.theta.length
    val covmat: DenseMatrix[Double] = covariance(array2DToMatrix(previouslyAccepted.map(_.simulation.theta))) :* 2.0
    val multi = math.exp(-0.5 * nbParam * math.log(2 * math.Pi)) / math.sqrt(math.abs(det(covmat)))
    val invmat = inv(covmat) :* 0.5
    val weights = for (rowNew <- 0 until newAccepted.length) yield {
      // k
      (for (rowPrevious <- 0 until previouslyAccepted.length) yield {
        // i
        val tmp = DenseVector((0 until nbParam).map(col => newAccepted(rowNew).theta(col) - previouslyAccepted(rowPrevious).simulation.theta(col)).toArray)
        previouslyAccepted(rowPrevious).weight * math.exp(-(tmp.t * invmat * tmp).valueAt(0))
      }).sum
    }
    val tab_weight_prior = computeWeightsPrior(newAccepted, priors)
    (tab_weight_prior zip weights).map {
      case (twp: Double, w: Double) => twp / (w * multi)
    }
  }

  def sample(
    previousState: LenormanState,
    nbSimus: Int)(implicit rng: Random): Seq[Seq[Double]] = {

    val n_alpha = math.ceil(nbSimus * alpha).toInt
    val nbSimusStep = if (previousState.accepted == None) nbSimus else nbSimus - n_alpha

    previousState.accepted match {
      case None =>
        lhs(nbSimusStep, priors.length).map {
          row =>
            (row zip priors).map {
              case (sample, prior) =>
                prior.min + sample * (prior.max - prior.min)
            }
        }
      case Some(accepted) => (0 until nbSimusStep).map(_ => move(accepted))
    }
  }

  def analyse(
    nbSimus: Int,
    previousState: LenormanState,
    thetas: Seq[Seq[Double]],
    summaryStats: Seq[Seq[Double]]): STATE = {
    val n_alpha = math.ceil(nbSimus * alpha).toInt
    // determination of the normalization constants in each dimension associated to each summary statistic, this normalization will not change during all the algorithm
    val varSummaryStats =
      previousState.varSummaryStats.getOrElse(
        for {
          col <- 0 until summaryStatsTarget.length
        } yield math.min(1.0, 1 / new DescriptiveStatistics(summaryStats.map(_(col)).toArray).getVariance)
      )
    // selecting the simulations
    val acceptedRaw =
      previousState.accepted match {
        case None =>
          // initial step: all simulations are kept
          for {
            (t, ss) <- thetas zip summaryStats
          } yield WeightedSimulation(new Simulation(t, ss, distance(ss, varSummaryStats)), weight = 1 / thetas.length.toDouble)
        case Some(accepted) =>
          // following steps: computing distances and weights
          val newSimulations =
            (for ((t, ss) <- (thetas, summaryStats).zipped) yield Simulation(t, ss, distance(ss, previousState.varSummaryStats.get))).toSeq

          val weights = computeWeights(accepted, newSimulations)
          // keeping only new simulations under tolerance threshold
          val newAccepted =
            for {
              (s, w) <- newSimulations zip weights
              if s.distance <= previousState.tolerance
            } yield WeightedSimulation(s, w / weights.sum)
          accepted ++ newAccepted
      }

    val accepted = acceptedRaw.sortWith(_.simulation.distance < _.simulation.distance).slice(0, n_alpha)
    val nextTolerance = accepted.last.simulation.distance
    val proportionOfAccepted =
      previousState.accepted match {
        case None => previousState.proportionOfAccepted
        case Some(previous) => (acceptedRaw.length - previous.length).toDouble / thetas.length
      }
    LenormanState(
      previousState.iteration + 1,
      nextTolerance,
      Some(accepted),
      Some(varSummaryStats),
      proportionOfAccepted,
      thetas.size,
      previousState.evaluations + thetas.size
    )
  }

  override def step(previousState: STATE)(implicit rng: Random): STATE = {
    // sampling thetas
    val thetas = sample(previousState, simulations)
    // running simulations
    val summaryStats = runSimulations(thetas)
    analyse(simulations, previousState, thetas, summaryStats)
  }

  def computeWeights(
    simulations: Seq[Simulation],
    previousState: State,
    varSummaryStats: Seq[Double],
    thetas: Seq[Seq[Double]],
    summaryStats: Seq[Seq[Double]],
    tolerance: Double): Seq[WeightedSimulation] =
    previousState.accepted match {
      case None =>
        simulations.map(WeightedSimulation(_, weight = 1 / thetas.length.toDouble))
      case Some(accepted) =>
        val weights = computeWeights(accepted, simulations)
        val sumWeights = weights.sum
        for {
          (s, w) <- simulations zip weights
        } yield WeightedSimulation(s, w / sumWeights)
    }

}
