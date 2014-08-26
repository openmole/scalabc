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

import fr.iscpif.scalabc.model.examples.{ TraitModel, ToyModel }
import fr.iscpif.scalabc.prior.Uniform
import org.apache.commons.math3.random.{ RandomAdaptor, MersenneTwister }
import fr.iscpif.scalabc.distance.DefaultDistance
import java.io.{ File, PrintWriter }
import fr.iscpif.scalabc._
import fr.iscpif.scalabc.sampling.JabotMover
import fr.iscpif.scalabc.executer.SequentialExecuter
import fr.iscpif.scalabc.algorithm.{ Lenormand, Beaumont, State }

object Test extends App {

  def printState(s: State) = println("#" + s.iteration + " " + s.accepted.getOrElse(List()).length + "/"
    + s.evaluationsForStep + "/" + s.evaluations + " tol=" + s.tolerance + "\n"
    + s.accepted.getOrElse(List()).mkString("\n"))

  def writeState(filesPrefix: String, s: State) = {
    val pw = new PrintWriter(new File(filesPrefix + s.iteration))
    try pw.write(s.accepted.get.mkString("\n"))
    finally pw.close()
  }

  // init a RNG
  implicit val rng = new util.Random(new RandomAdaptor(new MersenneTwister(1)))

  // a test on our model
  //println(toyModel.apply(Seq(2.0, 3.0), 1))

  // initialization of Lenormand algorithm
  val maxToy = new LenormandExecuter with JabotMover with DefaultDistance with ToyModel {
    def summaryStatsTarget = Seq(5, 5)
    def simulations = 10
    def priors = Seq(Uniform(0.0, 10.0), Uniform(0.0, 10.0))
  }
  //run the algorithm
  maxToy.run.foreach(printState)

  // initialization of Beaumont algorithm
  val abcToy = new BeaumontExecuter with JabotMover with DefaultDistance with ToyModel {
    def tolerances = Seq(5, 1, 0.5)
    def summaryStatsTarget = Seq(5, 5)
    def simulations = 10
    def priors = Seq(Uniform(0.0, 10.0), Uniform(0.0, 10.0))
  }

  abcToy.run.foreach(printState)

  // a test on our model
  //println(traitModel.apply(Seq(4, 1, 0.5, -0.1), 1))

  // initialization of Beaumont algorithm
  val abcTrait = new BeaumontExecuter with JabotMover with DefaultDistance with TraitModel {
    def tolerances = Seq(80, 50, 20)
    def summaryStatsTarget = Seq(100, 2.5, 20, 30000)
    def simulations = 5
    def priors = Seq(Uniform(3.0, 5.0), Uniform(-2.3, 1.6), Uniform(-25, 125), Uniform(-0.7, 3.2))
  }
  //run the algorithm
  abcTrait.run.foreach(printState)

  // initialization of Lenormand algorithm
  val maxTrait = new LenormandExecuter with JabotMover with DefaultDistance with TraitModel {
    def summaryStatsTarget = Seq(100, 2.5, 20, 30000)
    def simulations = 20
    def priors = Seq(Uniform(3.0, 5.0), Uniform(-2.3, 1.6), Uniform(-25, 125), Uniform(-0.7, 3.2))
  }
  //run the algorithm
  maxTrait.run.foreach(printState)

}
