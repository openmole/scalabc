package fr.iscpif.scalabc

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
import breeze.linalg.{ DenseMatrix, DenseVector }

@RunWith(classOf[JUnitRunner])
class ToolsTest extends FunSuite {

  test("cov") {
    val xy = DenseMatrix((1 to 10).toArray.map(_.toDouble), Array.concat((1 to 3).toArray, (5 to 8).toArray.reverse, (8 to 10).toArray).map(_.toDouble)).t
    val w1 = DenseVector(0.0, 0.0, 0.0, 0.2, 0.2, 0.2, 0.2, 0.2, 0.0, 0.0)
    val covmat = covarianceWeighted(xy, w1)
    // expected result obtained with the function cov.wt in R 2.15.2
    val expected = DenseMatrix((2.5, -0.5), (-0.5, 1.7))
    (covmat.data zip expected.data).map {
      case (x1, x2) => assert(math.abs(x1 - x2) <= 0.001)
    }
  }
}
