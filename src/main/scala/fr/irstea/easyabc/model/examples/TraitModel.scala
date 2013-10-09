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

trait TraitModel <: Model {

  def run(thetas: Array[Double]): Array[Double] = {
    val file1 = new File("./input")
    val writer = new FileWriter(file1)
    writer.write(1 + "\n" + thetas(0).toInt + "\n" + thetas.slice(1, thetas.length).mkString("\n") + "\n")
    writer.close
    // TODO invoke the model execution
    //"./trait_model".!!
    val file = Source.fromFile("./output")
    try {
      val data = file.getLines.mkString
      data.split("\\s+").map(_.toDouble)
    } finally {
      file.close
    }
  }

}
