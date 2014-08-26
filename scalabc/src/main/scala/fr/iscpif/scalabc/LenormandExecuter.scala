/*
 * Copyright (C) 16/01/14 Romain Reuillon
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

package fr.iscpif.scalabc

import fr.iscpif.scalabc.executer.SequentialExecuter
import fr.iscpif.scalabc.algorithm.Lenormand
import scala.util.Random

trait LenormandExecuter <: Lenormand with SequentialExecuter {

  override def step(previousState: STATE)(implicit rng: Random): STATE = {
    // sampling thetas
    val thetas = sample(previousState)
    // running simulations
    val summaryStats = runSimulations(thetas)
    analyse(previousState, thetas, summaryStats)
  }
}
