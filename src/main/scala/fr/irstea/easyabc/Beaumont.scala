package fr.irstea.easyabc

import fr.irstea.easyabc.model.Model
import fr.irstea.easyabc.model.prior.PriorFunction
import fr.irstea.easyabc.distance.{DefaultDistance, DistanceFunction}
import scala.collection.mutable.ArrayBuffer
import org.apache.commons.math3.random.RandomGenerator
import scala.Some
import fr.irstea.easyabc.sampling.{JabotMoving, ParticleMover}
import breeze.stats.DescriptiveStats

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

  def computeWeights(previouslyAccepted: Seq[WeightedSimulation], newAccepted: Seq[Simulation]): Seq[WeightedSimulation] = {
    val nbParam = previouslyAccepted(0).simulation.theta.length
    val nbParticle = previouslyAccepted.length
    val nbNewParticle = newAccepted.length
    val var_array = (0 until nbParam).map(col => DescriptiveStats.variance(previouslyAccepted.map(_.simulation.theta(col))))
    val multi = var_array.foldLeft(math.pow(1 / math.sqrt(2 * math.Pi), nbParam))((s, t) => s * 1 / math.sqrt(t / 2))
    val weights = Array.fill(nbParticle)(0.0)
    for (i <- 0 until nbParticle) {
      var tab_temp = Array.fill(nbNewParticle)(weights(i) * multi)
      for (k <- 0 until nbParam) {
        // TODO tab_temp = tab_temp.map(_ * math.exp(-newAccepted()))
        // tab_temp=tab_temp*exp(-(as.numeric(param_simulated[,k])-as.numeric(param_previous_step[i,k]))*(as.numeric(param_simulated[,k])-as.numeric(param_previous_step[i,k]))/var_array[k])
      }
      // tab_weight_new=tab_weight_new+tab_temp
    }
    // tab_weight_prior=.compute_weight_prior_tab(param_simulated,prior)
    // tab_weight_new=tab_weight_prior/tab_weight_new
    for ((s, w) <- newAccepted zip weights) yield WeightedSimulation(s, w / weights.sum)
  }

  def apply(model: Model, useSeed: Boolean = false, prior: Seq[PriorFunction[Double]], nbSimus: Int, distanceFunction: DistanceFunction = new DefaultDistance(summaryStatsTarget), particleMover: ParticleMover = new JabotMoving()) = {
    currentStep = 0
    var tolerance = nextTolerance()
    var accepted: Seq[WeightedSimulation] = Nil
    while (tolerance != None) {
      val newAccepted = ArrayBuffer.empty[Simulation]
      var currentSeed = 0
      // TODO run several (nbSimus - newAccepted.size) simulations at a time, for further parallelisation
      while (newAccepted.size < nbSimus) {
        var theta: Seq[Double] = Nil
        if (currentStep == 0) {
          theta = for (p <- prior) yield p.value()
        } else {
          theta = particleMover.move(accepted)
        }
        val summaryStats = model.apply(theta, if (useSeed) Some(currentSeed) else None)
        val distance = distanceFunction.distance(summaryStats)
        if (distance <= tolerance.get) {
          newAccepted += new Simulation(theta, summaryStats, distance)
        }
        currentSeed += 1
      }
      // compute weights
      val distanceMax: Double = newAccepted.foldLeft(0.0)(_ max _.distance)
      // TODO fonction computeWeights
      // TODO prendre une meilleure formule de calcul de poids
      val weights: Seq[Double] =
        if (currentStep == 0) {
          Array.fill(nbSimus)(1 / nbSimus.toDouble)
        } else {
          for (s <- newAccepted) yield 1 - math.pow(s.distance / distanceMax, 2)
        }
      val sumWeights = weights.sum
      accepted = for ((s, w) <- newAccepted zip weights) yield WeightedSimulation(s, w / sumWeights)
      // go to the next tolerance
      currentStep += 1
      tolerance = nextTolerance()
      println("Accepted at the end of iteration " + currentStep + " : " + accepted.mkString("\n"))
      // TODO écrire les fichiers
    }

  }
}
