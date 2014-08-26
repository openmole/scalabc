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
import org.apache.commons.math3.random.MersenneTwister
import fr.irstea.scalabc.sampling.JabotMover
import fr.irstea.scalabc.distance.DefaultDistance
import fr.irstea.scalabc.prior.Uniform
import fr.irstea.scalabc.algorithm.{ Lenormand, WeightedSimulation, Simulation }

@RunWith(classOf[JUnitRunner])
class LenormandTest extends FunSuite {

  test("computeWeights") {
    val previouslyAccepted = List(
      WeightedSimulation(Simulation(List(1.5601863631356871, 1.5599452108107381), List(3.8476952539792935, 3.1170287169055837), 3.035276029115123), 0.1),
      WeightedSimulation(Simulation(List(1.8182496714907215, 1.834045144540295), List(4.379858496063885, 4.01797545331939), 1.6021660506167255), 0.1),
      WeightedSimulation(Simulation(List(5.924145648891164, 0.46450419127052944), List(7.116213520194562, 3.4350139553668613), 3.681199564827701), 0.1),
      WeightedSimulation(Simulation(List(3.0461376929232853, 0.9767210827609629), List(4.750422455717117, 3.6584503774308583), 1.5911271668520248), 0.1),
      WeightedSimulation(Simulation(List(1.22038239389608, 4.951769088684754), List(6.8997151626137025, 6.726275286229557), 3.6259904488432593), 0.1),
      WeightedSimulation(Simulation(List(0.8849250630205829, 1.9598286629966277), List(3.572317406050079, 2.417524974871681), 4.010157619078241), 0.1),
      WeightedSimulation(Simulation(List(0.4522728862124703, 3.2533032960495123), List(4.43313986229485, 2.154604343188701), 3.4122557945164487), 0.1),
      WeightedSimulation(Simulation(List(3.584657259445938, 1.1586905889152987), List(5.470911528394105, 4.83673210276676), 0.6341794256273454), 0.1),
      WeightedSimulation(Simulation(List(3.308980181129293, 0.6355834559825424), List(4.672127317144703, 2.7863565310597402), 2.5415161517955567), 0.1),
      WeightedSimulation(Simulation(List(0.31429184910112484, 6.364104073549839), List(7.405959602683832, 2.6834095089078254), 4.7225500937760065), 0.1)
    )
    val newAccepted = List(
      Simulation(List(2.4141840738529794, 1.599067140327586), List(4.740814894213433, 4.5436658949603315), 0.7155192108262352),
      Simulation(List(3.1637184591499863, 1.285279512938156), List(5.17656165212101, 4.7494859920095935), 0.4270756601114165),
      Simulation(List(3.3449403422904935, 1.1316263949614611), List(5.204130417284823, 4.468446252667192), 0.7356841646176306),
      Simulation(List(3.0728365282363956, 1.2548957805681145), List(5.055295988837378, 4.539313065419272), 0.5159829234181066),
      Simulation(List(1.3776537944319944, 3.0850750247806915), List(5.190292499245554, 4.933388785756344), 0.2569037134892094),
      Simulation(List(3.581181341075928, 1.0591641283239452), List(5.367909149432742, 4.476282285250508), 0.8916268641822338),
      Simulation(List(2.373949181135898, 1.8544618842779972), List(4.955974745446763, 5.085621743389331), 0.12964699794256784),
      Simulation(List(2.9609736510879467, 1.593026341420852), List(5.281563672541667, 5.400132494196019), 0.6816961667376864),
      Simulation(List(1.9404237033515908, 2.4870501920126866), List(5.155037575397146, 5.509154615766388), 0.6641921911635338),
      Simulation(List(2.3285151599006944, 2.1296553040834114), List(5.1857341440169735, 5.642158132680992), 0.8278922766979653)
    )
    implicit val rng = new MersenneTwister(42)
    val lenormand = new Lenormand with JabotMover with DefaultDistance {
      def summaryStatsTarget = Seq(5, 5)
      def simulations = ???
      def priors = Seq(Uniform(0.0, 10.0), Uniform(0.0, 10.0))
    }
    val weights = lenormand.computeWeights(previouslyAccepted, newAccepted)
    // results computed in R with EasyABC 1.2.2
    val expectedWeights = Seq(0.4316663, 0.4573844, 0.4679478, 0.4518792, 0.4556170, 0.4866968, 0.4309326, 0.4505411, 0.4362668, 0.4349588)
    (weights zip expectedWeights).map {
      case (x1, x2) => assert(math.abs(x1 - x2) / x1 <= 0.01)
    }
  }
}
