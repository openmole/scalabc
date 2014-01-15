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
trait DefaultDistance <: Distance {

  def distance(summaryStats: Seq[Double], initVariance: Seq[Double]): Double = {
    (initVariance, summaryStats, summaryStatsTarget).zipped.map {
      case (v, ss, sst) => v * (ss - sst) * (ss - sst)
    }.sum
  }
}
