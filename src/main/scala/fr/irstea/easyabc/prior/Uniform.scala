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

package fr.irstea.easyabc.prior

import org.apache.commons.math3.random.RandomDataGenerator
import scala.util.Random
import fr.irstea.easyabc._

case class Uniform(min: Double, max: Double) extends PriorFunction[Double] {
  def value(implicit rng: Random) = new RandomDataGenerator(rng).nextUniform(min, max)
  def density(value: Double): Double = 1 / (max - min)
}