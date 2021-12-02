package databrewers
package adventofcode

import scala.io._

object Day02 extends App {

  val inputs = Source.fromResource("Day02.txt").getLines.toList

  println(inputs)


  val regex = "(\\w+) ([\\d]+)".r


  def extractCoordinates(text: String): Coordinates = {
    text match {
        case regex(action, amount) if action == "forward" => Coordinates(amount.toInt, 0, 0)
        case regex(action, amount) if action == "up" => Coordinates(0, amount.toInt * -1, 0)
        case regex(action, amount) if action == "down" => Coordinates(0, amount.toInt, 0)
    }
  }

  val coordinates = inputs
    .map(extractCoordinates)

  val endCoordinates = coordinates match {
      case head :: rest => rest.foldLeft(head)(_ ++ _)
      case Nil => Coordinates(0, 0, 0)
  }

  val endCoordinates2 = coordinates match {
      case head :: rest => rest.foldLeft(head)(_ ** _)
      case Nil => Coordinates(0, 0, 0)
  }

  println(endCoordinates)
  println(endCoordinates.horizontal * endCoordinates.vertical)

  println(endCoordinates2)
  println(endCoordinates2.horizontal * endCoordinates2.vertical)


}

case class Coordinates(horizontal: Int, vertical: Int, aim: Int) {

    def ++(other: Coordinates): Coordinates = {

        Coordinates(
            horizontal = horizontal + other.horizontal,
            vertical = vertical + other.vertical,
            aim = aim + other.aim
        )

    }

    def **(other: Coordinates): Coordinates = {

        Coordinates(
            horizontal = horizontal + other.horizontal,
            vertical = vertical + (other.horizontal * aim),
            aim = aim + other.vertical
        )

    }



}