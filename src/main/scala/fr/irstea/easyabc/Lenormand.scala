package fr.irstea.easyabc

import fr.irstea.easyabc.model.Model
import fr.irstea.easyabc.model.prior.{Uniform, PriorFunction}
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
class Lenormand(val alpha: Double = 0.5, val pAccMin: Double = 0.05, val summaryStatsTarget: Seq[Double])(implicit rng: RandomGenerator) extends SequentialABC {

  override def tolerancesIt(): Iterator[Double] = {
    new Iterator[Double] {
      def hasNext: Boolean = pAcc > pAccMin

      def next(): Double = tolNext
    }
  }

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

  var pAcc = pAccMin + 1
  var tolNext = 0.0

  override def step(model: Model, priors: Seq[PriorFunction[Double]], nbSimus: Int, tolerance: Double,
                    previousState: State,
                    distanceFunction: DistanceFunction,
                    particleMover: ParticleMover): State = {
    val n_alpha = math.ceil(nbSimus * alpha).toInt
    if (previousState.accepted == None) {
      // Initial step
      val thetas = Tools.lhs(nbSimus, priors.length).map {
        row =>
          (row zip priors).map {
            case (sample, prior) => {
              val unif = prior.asInstanceOf[Uniform]
              unif.min + sample * (unif.max - unif.min)
            }
          }
      }
      // running simulations
      val summaryStats = runSimulations(model, thetas, 0 until nbSimus)
      var currentSeed = nbSimus
      val varSummaryStats = for (col <- 0 until summaryStatsTarget.length) yield math.min(1.0, 1 / new DescriptiveStatistics(summaryStats.map(_(col)).toArray).getVariance)
      val accepted = {
        for ((t, ss) <- thetas zip summaryStats) yield {
          new WeightedSimulation(new Simulation(t, ss, distanceFunction.distance(ss, varSummaryStats)), weight = 1 / nbSimus.toDouble)
        }
      }.sortWith(_.simulation.distance < _.simulation.distance).slice(0, n_alpha)
      tolNext = accepted.last.simulation.distance
      new State(previousState.iteration + 1, nbSimus, nbSimus, tolerance, Some(accepted), Some(varSummaryStats))
    } else {
      // Following steps
      val nbSimusStep = nbSimus - n_alpha
      val thetas = (0 until nbSimusStep).map(_ => particleMover.move(previousState.accepted.get))
      // init seeds
      val seeds = (0 until nbSimusStep).map(_ + previousState.nbSimulatedTotal)
      // running simulations
      val summaryStats = runSimulations(model, thetas, seeds)
      val newSimulations = (for ((t, ss) <- (thetas, summaryStats).zipped) yield new Simulation(t, ss, distanceFunction.distance(ss, previousState.varSummaryStats.get))).toSeq
      val weights = computeWeights(previousState.accepted.get, newSimulations, priors)
      val newAccepted = {
        for ((s, w) <- newSimulations zip weights if s.distance <= tolerance) yield new WeightedSimulation(s, w / weights.sum)
      }
      pAcc = newAccepted.length.toDouble / nbSimusStep
      val accepted = (previousState.accepted.get ++ newAccepted).sortWith(_.simulation.distance < _.simulation.distance).slice(0, n_alpha)
      tolNext = accepted.last.simulation.distance
      new State(previousState.iteration + 1, nbSimusStep, previousState.nbSimulatedTotal + nbSimusStep, tolerance,
        Some(accepted),
        previousState.varSummaryStats)
    }
  }

}
