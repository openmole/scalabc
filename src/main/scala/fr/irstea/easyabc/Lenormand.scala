package fr.irstea.easyabc

import fr.irstea.easyabc.model.Model
import fr.irstea.easyabc.model.prior.{ Uniform, PriorFunction }
import fr.irstea.easyabc.distance.DistanceFunction
import fr.irstea.easyabc.sampling.ParticleMover
import org.apache.commons.math3.random.RandomGenerator
import org.apache.commons.math3.stat.descriptive.DescriptiveStatistics
import breeze.linalg._
import fr.irstea.easyabc.Tools._

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

case class LenormanState(
  iteration: Int,
  nbSimulatedThisStep: Int,
  nbSimulatedTotal: Int,
  tolerance: Double,
  accepted: Option[Seq[WeightedSimulation]],
  varSummaryStats: Option[Seq[Double]],
  proportionOfAccepted: Double) extends State

class Lenormand(val alpha: Double = 0.5, val pAccMin: Double = 0.05, val summaryStatsTarget: Seq[Double])(implicit rng: RandomGenerator) extends SequentialABC {

  type STATE = LenormanState

  def initialState = LenormanState(
    iteration = 0,
    nbSimulatedThisStep = 0,
    nbSimulatedTotal = 0,
    tolerance = 0.0,
    None,
    None,
    pAccMin + 1)

  def finished(s: STATE) = s.proportionOfAccepted <= pAccMin

  override def computeWeights(previouslyAccepted: Seq[WeightedSimulation], newAccepted: Seq[Simulation], priors: Seq[PriorFunction[Double]]): Seq[Double] = {
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
      case (twp: Double, w: Double) => {
        twp / (w * multi)
      }
    }
  }

  override def sample(previousState: LenormanState, nbSimus: Int, seedIndex: Int, priors: Seq[PriorFunction[Double]], particleMover: ParticleMover): (Seq[Seq[Double]], Seq[Int]) = {
    if (previousState.accepted == None)
      (Tools.lhs(nbSimus, priors.length).map {
        row =>
          (row zip priors).map {
            case (sample, prior) => {
              val unif = prior.asInstanceOf[Uniform]
              unif.min + sample * (unif.max - unif.min)
            }
          }
      }, 0 until nbSimus)
    else (
      (0 until nbSimus).map(_ => particleMover.move(previousState.accepted.get)),
      (0 until nbSimus).map(_ + seedIndex)
    )
  }

  def analyse(
    priors: Seq[PriorFunction[Double]],
    nbSimus: Int,
    previousState: LenormanState,
    distanceFunction: DistanceFunction,
    thetas: Seq[Seq[Double]],
    summaryStats: Seq[Seq[Double]]): STATE = {
    val n_alpha = math.ceil(nbSimus * alpha).toInt
    // determination of the normalization constants in each dimension associated to each summary statistic, this normalization will not change during all the algorithm
    val varSummaryStats = previousState.varSummaryStats.getOrElse(
      for (col <- 0 until summaryStatsTarget.length) yield math.min(1.0, 1 / new DescriptiveStatistics(summaryStats.map(_(col)).toArray).getVariance)
    )
    // selecting the simulations
    val acceptedRaw =
      if (previousState.accepted == None) {
        // initial step: all simulations are kept
        for {
          (t, ss) <- thetas zip summaryStats
        } yield WeightedSimulation(new Simulation(t, ss, distanceFunction.distance(ss, varSummaryStats)), weight = 1 / thetas.length.toDouble)
      } else {
        // following steps: computing distances and weights
        val newSimulations = (for ((t, ss) <- (thetas, summaryStats).zipped) yield new Simulation(t, ss, distanceFunction.distance(ss, previousState.varSummaryStats.get))).toSeq
        val weights = computeWeights(previousState.accepted.get, newSimulations, priors)
        // keeping only new simulations under tolerance threshold
        val newAccepted =
          for {
            (s, w) <- newSimulations zip weights
            if s.distance <= previousState.tolerance
          } yield WeightedSimulation(s, w / weights.sum)
        previousState.accepted.get ++ newAccepted
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
      thetas.length,
      previousState.nbSimulatedTotal + thetas.length,
      nextTolerance,
      Some(accepted),
      Some(varSummaryStats),
      proportionOfAccepted)
  }

  override def step(
    model: Model,
    priors: Seq[PriorFunction[Double]],
    nbSimus: Int,
    previousState: STATE,
    distanceFunction: DistanceFunction,
    particleMover: ParticleMover): STATE = {
    val n_alpha = math.ceil(nbSimus * alpha).toInt
    val nbSimusStep = if (previousState.accepted == None) nbSimus else nbSimus - n_alpha
    // sampling thetas and init seeds
    val (thetas, seeds) = sample(previousState, nbSimusStep, previousState.nbSimulatedTotal, priors, particleMover)
    println(thetas.length+" simulations")
    // running simulations
    val summaryStats = runSimulations(model, thetas, seeds)
    analyse(priors, nbSimus, previousState, distanceFunction, thetas, summaryStats)
  }

  def computeWeights(simulations: Seq[Simulation], previousState: State, varSummaryStats: Seq[Double], thetas: Seq[Seq[Double]], summaryStats: Seq[Seq[Double]],
    tolerance: Double, distanceFunction: DistanceFunction, priors: Seq[PriorFunction[Double]]): Seq[WeightedSimulation] = {
    if (previousState.accepted == None) {
      simulations.map(new WeightedSimulation(_, weight = 1 / thetas.length.toDouble))
    } else {
      val weights = computeWeights(previousState.accepted.get, simulations, priors)
      val sumWeights = weights.sum
      for ((s, w) <- simulations zip weights) yield WeightedSimulation(s, w / sumWeights)
    }
  }

}
