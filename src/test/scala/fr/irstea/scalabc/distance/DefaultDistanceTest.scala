package fr.irstea.scalabc.distance

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

import org.scalatest.junit.JUnitRunner
import org.scalatest.FunSuite
import org.junit.runner.RunWith

@RunWith(classOf[JUnitRunner])
class DefaultDistanceTest extends FunSuite {

  test("distance") {
    val target = Seq(0.0, 10.0)
    val stats = Seq(-1.0, 9.0)
    val variance = Seq(0.02, 0.02)
    assert(
      math.abs(
        new DefaultDistance {
          def summaryStatsTarget = target
        }.distance(stats, variance) - 0.04) < 0.000001)
  }
}