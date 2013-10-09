package fr.irstea.easyabc.model.examples

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

import fr.irstea.easyabc.model.Model

trait ToyModel <: Model {

  def apply(thetas: Seq[Double], seed: Option[Int]=None): Seq[Double] = {
    val random = new util.Random(seed match {
      case Some(s) => s
      case None => 42
    })
    Seq(thetas(0) + thetas(1) + random.nextDouble, thetas(0) * thetas(1) + random.nextDouble)
  }

}
