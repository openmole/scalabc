package fr.irstea.easyabc.sampling

import fr.irstea.easyabc.WeightedSimulation
import fr.irstea.easyabc.Tools._
import breeze.linalg._
import org.apache.commons.math3.random.{ RandomDataGenerator, RandomGenerator }
import breeze.numerics

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

class JabotMoving extends ParticleMover {
  /**
   * Implementation based on those from Jabot in EasyABC R package
   * @param simulations
   * @param rng
   * @return
   */
  // TODO add the option for keeping the particle inside the priors
  def move(simulations: Seq[WeightedSimulation])(implicit rng: RandomGenerator): Seq[Double] = {
    val sd: DenseVector[Double] = diag(covarianceWeighted(array2DToMatrix(simulations.map(_.simulation.theta)), DenseVector((for (s <- simulations) yield s.weight).toArray))) * 2.0
    val sd2: DenseVector[Double] = numerics.sqrt(sd)
    val paramPicked = pickTheta(simulations).simulation.theta
    val rdg = new RandomDataGenerator(rng)
    (paramPicked zip sd2.data).map {
      case (p, s) => rdg.nextGaussian(p, s)
    }
  }
}
