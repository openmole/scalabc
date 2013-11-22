package fr.irstea.easyabc

import fr.irstea.easyabc.model.Model
import fr.irstea.easyabc.model.prior.PriorFunction
import fr.irstea.easyabc.distance.{DefaultDistance, DistanceFunction}
import scala.collection.mutable.ListBuffer
import org.apache.commons.math3.random.RandomGenerator
import scala.Some
import fr.irstea.easyabc.sampling.{JabotMoving, ParticleMover}
import breeze.stats.DescriptiveStats
import breeze.linalg.{pow => bpow, DenseVector}
import breeze.numerics.{exp => bexp}
import fr.irstea.easyabc.output.{PrinterHandler, Handler}
import org.apache.commons.math3.stat.descriptive.DescriptiveStatistics

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
    if (currentStep >= tolerances.length) {
      None
    } else {
      Some(tolerances(currentStep))
    }
  }

  /**
   * computes particle weights with unidimensional jumps
   */
  def computeWeights(previouslyAccepted: Seq[WeightedSimulation], newAccepted: Seq[Simulation], priors: Seq[PriorFunction[Double]]): Seq[Double] = {
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

  def selectSimulation(thetas: Seq[Seq[Double]], summaryStats: Seq[Seq[Double]], var_summaryStats: Seq[Double], tolerance: Double): Seq[Simulation] = {
    val simus: Seq[Simulation] = for ((theta, summaryStat) <- thetas zip summaryStats) yield {
      new Simulation(theta, summaryStat, distance = (for ((v, ss, sst) <- (var_summaryStats, summaryStat, summaryStatsTarget).zipped) yield v * (ss - sst) * (ss - sst)).sum)
    }
    //simus.map(s => println(s.theta+" = "+s.summaryStats+" -> "+s.distance))
    simus.filter(_.distance < tolerance)
  }

  // TODO implementation with Iterator
  /*def init():Seq[WeightedSimulation] = {
         Seq.empty[WeightedSimulation]
  }
  def step():Seq[WeightedSimulation] = {
      Seq.empty[WeightedSimulation]
  }

  def run():Iterator[Seq[WeightedSimulation]] = {
     Iterator.iterate(init) {
             _ =>  step
     }
  }*/

  def apply(model: Model, useSeed: Boolean = false, prior: Seq[PriorFunction[Double]], nbSimus: Int,
            distanceFunction: DistanceFunction = new DefaultDistance(summaryStatsTarget),
            particleMover: ParticleMover = new JabotMoving(),
            outputHandler: Handler = PrinterHandler) = {
    currentStep = 0
    var tolerance = nextTolerance()
    var accepted: Seq[WeightedSimulation] = Nil
    var var_summaryStats: Seq[Double] = Nil
    var currentSeed = 0
    while (tolerance != None) {
      var nbSimulated = 0
      val newAccepted = ListBuffer.empty[Simulation]
      while (newAccepted.size < nbSimus) {
        // we need this amount of accepted simulations to reach nbSimus
        val remainingSimusForThisStep = nbSimus - newAccepted.size
        // sampling thetas
        val thetas = (0 until remainingSimusForThisStep).map(_ => if (currentStep == 0) {
          for (p <- prior) yield p.value()
        } else {
          particleMover.move(accepted)
        })
        // init seeds
        val seeds = (0 until remainingSimusForThisStep).map(seed => if (useSeed) Some(seed + currentSeed) else None)
        // running simulations
        val summaryStats = runSimulations(model, thetas, seeds)
        // determination of the normalization constants in each dimension associated to each summary statistic, this normalization will not change during all the algorithm
        if (var_summaryStats == Nil) {
          var_summaryStats = for (col <- 0 until summaryStatsTarget.length) yield math.min(1.0, 1 / new DescriptiveStatistics(summaryStats.map(_(col)).toArray).getVariance)
        }
        // selecting the tolerable simulations
        for (s <- selectSimulation(thetas, summaryStats, var_summaryStats, tolerance.get)) {
          newAccepted += s
          //println("nbSimulated:nbAccepted/nbSimus " + nbSimulated + "/" + newAccepted.length + "/" + nbSimus + " - " + s.summaryStats + " -> " + s.distance + " (" + tolerance.get + ")")
        }
        nbSimulated += thetas.length
        currentSeed += remainingSimusForThisStep
      } // until we get nbSimus simulations below the tolerance threshold
      // compute weights
      val distanceMax: Double = newAccepted.foldLeft(0.0)(_ max _.distance)
      val weights: Seq[Double] =
        if (currentStep == 0) {
          // initial step
          Array.fill(nbSimus)(1 / nbSimus.toDouble)
        } else {
          // following steps
          computeWeights(accepted, newAccepted, prior)
        }
      val sumWeights = weights.sum
      accepted = for ((s, w) <- newAccepted zip weights) yield WeightedSimulation(s, w / sumWeights)
      // go to the next tolerance
      currentStep += 1
      tolerance = nextTolerance()
      outputHandler.handle(currentStep, accepted)
    }
  }
}
