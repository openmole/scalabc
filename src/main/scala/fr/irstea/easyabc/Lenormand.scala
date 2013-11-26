package fr.irstea.easyabc

import fr.irstea.easyabc.model.Model
import fr.irstea.easyabc.model.prior.{Uniform, PriorFunction}
import fr.irstea.easyabc.distance.{DefaultDistance, DistanceFunction}
import fr.irstea.easyabc.sampling.{JabotMoving, ParticleMover}
import fr.irstea.easyabc.output.{PrinterHandler, Handler}
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

  var currentStep = 0

  def computeWeights(previouslyAccepted: Seq[WeightedSimulation], newAccepted: Seq[Simulation], priors: Seq[PriorFunction[Double]]): Seq[Double] = {
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

  def apply(model: Model, priors: Seq[PriorFunction[Double]], nbSimus: Int,
            distanceFunction: DistanceFunction = new DefaultDistance(summaryStatsTarget),
            particleMover: ParticleMover = new JabotMoving(),
            outputHandler: Handler = PrinterHandler) = {
    currentStep = 0
    //////////////// Initial step
    val n_alpha = math.ceil(nbSimus * alpha).toInt
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
    val summaryStats = runSimulations(model, thetas, (0 until nbSimus))
    var currentSeed = nbSimus
    val var_summaryStats = for (col <- 0 until summaryStatsTarget.length) yield math.min(1.0, 1 / new DescriptiveStatistics(summaryStats.map(_(col)).toArray).getVariance)
    var accepted = {
      for ((t, ss) <- (thetas zip summaryStats)) yield {
        new WeightedSimulation(new Simulation(t, ss, distanceFunction.distance(ss, var_summaryStats)), weight = 1 / nbSimus.toDouble)
      }
    }.sortWith(_.simulation.distance < _.simulation.distance).slice(0, n_alpha)
    ///////////////// Following steps
    var tol_next = accepted.last.simulation.distance
    var pAcc = pAccMin + 1
    val nbSimusStep = nbSimus - n_alpha
    while (pAcc > pAccMin) {
      currentStep += 1
      val thetas = (0 until nbSimusStep).map(_ => particleMover.move(accepted))
      // init seeds
      val seeds = (0 until nbSimusStep).map(_ + currentSeed)
      currentSeed += nbSimusStep
      // running simulations
      val summaryStats = runSimulations(model, thetas, seeds)
      val newSimulations = (for ((t, ss) <- (thetas, summaryStats).zipped) yield new Simulation(t, ss, distanceFunction.distance(ss, var_summaryStats))).toSeq
      val weights = computeWeights(accepted, newSimulations, priors)
      val newAccepted = {
        for ((s, w) <- (newSimulations zip weights) if s.distance <= tol_next) yield new WeightedSimulation(s, w / weights.sum)
      }
      pAcc = newAccepted.length.toDouble / nbSimusStep
      accepted = (accepted ++ newAccepted).sortWith(_.simulation.distance < _.simulation.distance).slice(0, n_alpha)
      tol_next = accepted.last.simulation.distance
      println("pacc = " + pAcc + " tol = " + tol_next)
      outputHandler.handle(currentStep, accepted)
    }

  }

  def nextTolerance(): Option[Double] = None

}
