/*
 * Copyright (C) 15/01/14 Romain Reuillon
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Affero General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

package fr.irstea.scalabc.executer

import scala.util.Random
import fr.irstea.scalabc.model.Model
import fr.irstea.scalabc.algorithm.ABC
import fr.irstea.scalabc._

trait SequentialExecuter <: ABC with Model {

  def step(state: STATE)(implicit rng: Random): STATE

  def run(implicit rng: Random): Iterator[STATE] =
    Iterator.iterate(initialState)(step).takeWhileInclusive(!finished(_))

  def runSimulations(thetas: Seq[Seq[Double]])(implicit rng: Random): Seq[Seq[Double]] =
    (thetas.iterator zip Iterator.continually(rng.nextLong())).toSeq.map {
      case (theta, seed) => model(theta, seed)
    }
}
