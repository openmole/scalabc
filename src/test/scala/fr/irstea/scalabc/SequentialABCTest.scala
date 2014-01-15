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

import org.scalatest.junit.JUnitRunner
import org.scalatest.FunSuite
import org.junit.runner.RunWith
import org.apache.commons.math3.random.AbstractRandomGenerator

@RunWith(classOf[JUnitRunner])
class SequentialABCTest extends FunSuite {

  class FakeRandom extends AbstractRandomGenerator {
    val numbers = List(0.72, 0.4, 0.6, 0.99)
    var i = 0

    override def nextDouble(): Double = {
      i += 1
      numbers(i - 1)
    }

    def setSeed(seed: Long) = {}
  }

  test("pickTheta") {
    implicit val rng = apacheRandomToScalaRandom(new FakeRandom())
    val factor = 4
    val thetas = List(WeightedSimulation(null, factor * 0.5),
      WeightedSimulation(null, factor * 0.2),
      WeightedSimulation(null, factor * 0.3))
    assert(pickTheta(thetas) eq thetas(2))
    assert(pickTheta(thetas) eq thetas(0))
    assert(pickTheta(thetas) eq thetas(1))
    assert(pickTheta(thetas) eq thetas(2))
  }
}
