package fr.irstea.easyabc

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

import fr.irstea.easyabc.model.examples.{TraitModel, ToyModel}
import fr.irstea.easyabc.model.prior.Uniform
import org.apache.commons.math3.random.MersenneTwister
import fr.irstea.easyabc.output.{PrinterHandler, FileOutputHandler}

object Test extends App {

  // our model to explore
  val toyModel = new ToyModel

  // init a RNG
  implicit val rng = new MersenneTwister(42)

  // a test on our model
  println(toyModel.apply(Seq(2.0, 3.0)))

  // initialization of Beaumont algorithm
  val abcToy = new Beaumont(tolerances = Seq(5, 1, 0.5), summaryStatsTarget = Seq(5, 5))
  //run the algorithm
  abcToy.apply(model = toyModel,
    useSeed = true,
    prior = Seq(new Uniform(0.0, 10.0), new Uniform(0.0, 10.0)),
    nbSimus = 10,
    outputHandler = new FileOutputHandler("beaumont_output_") // remove this argument for writing the output on the console
  )

  val traitModel = new TraitModel
  // TODO give different seeds to trait

  // a test on our model
  println(traitModel.apply(Seq(500, 4, 1, 1, 0.5, -0.1)))

  // initialization of Beaumont algorithm
  val abcTrait = new Beaumont(tolerances = Seq(8, 5, 2), summaryStatsTarget = Seq(100,2.5,20,30000))
  // TODO seems to run indefinitely
  //run the algorithm
  abcTrait.apply(model = traitModel,
    useSeed = true,
    prior = Seq(new Uniform(3.0, 5.0), new Uniform(-2.3, 1.6), new Uniform(-25, 125), new Uniform(-0.7, 3.2)),
    nbSimus = 10,
    outputHandler = PrinterHandler
  )

}
