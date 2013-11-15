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
import java.io.{FileWriter, File}
import scala.io.Source
import sys.process._

class TraitModel extends Model {

  def apply(thetas: Seq[Double], seed: Option[Int]=None): Seq[Double] = {
    val inputFile = new File("./input")
    val writer = new FileWriter(inputFile)
    try {
      writer.write(1 + "\n500\n" + thetas.slice(0, 2).mkString("\n")+ "\n1\n" + thetas.slice(2, thetas.length).mkString("\n") + "\n")
    } finally {
      writer.close
    }
    "./trait_model".!!
    val outputFile = new File("./output")
    val outputSource = Source.fromFile(outputFile)
    try {
      val data = outputSource.getLines.mkString
      data.split("\\s+").map(_.toDouble)
    } finally {
      outputSource.close
      //inputFile.delete
      outputFile.delete
    }
  }
}

