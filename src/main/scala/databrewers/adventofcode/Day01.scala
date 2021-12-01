package databrewers
package adventofcode

import scala.io._

object Day01 extends App {

  def measurements(depths: List[Int]): Int = {

    val list: List[Int] = depths match {
      case head :: rest => {
        rest
          .foldLeft(head -> List.empty[Int]) { case ((previous, increases), next) =>
            ((next, compare(previous, next) :: increases))
          }
          ._2
      }
      case Nil => List.empty
    }
    list.sum
  }

  def compare(previous: Int, next: Int) = {
    (previous, next) match {
      case _ if previous < next => 1
      case _                    => 0
    }
  }

  val inputs: List[Int] = Source.fromResource("Day01.txt").getLines.toList.map(_.toInt)

  println(measurements(inputs))

  val grouped = inputs
    .sliding(3)
    .filter(_.size == 3)
    .toList
    .map(_.sum)

  println(measurements(grouped))

}
