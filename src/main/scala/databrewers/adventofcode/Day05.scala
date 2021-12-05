package databrewers
package adventofcode

import scala.io._
import scala.collection.BitSet
import scala.util.matching.Regex

object Day05 extends App {

  case class Coordinates(x: Int, y: Int) {

    def swap: Coordinates = Coordinates(y, x)

  }

  def horizontal(fixed: Int, beginning: Int, end: Int): Seq[Coordinates] = {
    val byNum = if (beginning > end) -1 else 1
    for {
      num <- beginning to end by byNum
    } yield Coordinates(fixed, num)
  }

  def diagonal(start: Coordinates, end: Coordinates): List[Coordinates] = {
    val diagonals = {

      val factor = if (start.y >= end.y) -1 else 1
      val compY = Integer.min(start.y, end.y)

      for {
        index <- 0 to (end.x - start.x)
        y = start.y + (index * factor)
        x = start.x + index
        result = Coordinates(x, y) if y >= compY
      } yield result

    }

    diagonals.toList match {
      case Nil                      => Nil
      case list if list.last == end => list
      case _                        => Nil
    }
  }

  val inputs: List[String] = Source.fromResource("Day05.txt").getLines().filter(_.nonEmpty).toList

  val coordsR = "(\\d+),(\\d+) -> (\\d+),(\\d+)".r

  val coordinates = inputs.map { line =>
    line match {
      case coordsR(x1, y1, x2, y2) => Coordinates(x1.toInt, y1.toInt) -> Coordinates(x2.toInt, y2.toInt)
    }
  }

  val horizontals: List[Coordinates] = coordinates
    .foldLeft(List.empty[Coordinates]) {
      case (accum, (left, right)) if (left.x == right.x) => horizontal(left.x, left.y, right.y).toList ++ accum
      case (accum, (left, right)) if (left.y == right.y) =>
        horizontal(left.y, left.x, right.x).toList.map(_.swap) ++ accum
      case (accum, _) => accum
    }

  val result1 = horizontals.groupBy(identity).filter { case (keys, values) => values.size >= 2 }.keys.size
  println(result1)

  val diagonals = coordinates.flatMap {
    case (left, right) if left.x < right.x => diagonal(left, right)
    case (left, right)                     => diagonal(right, left)
  }

  val result2 = (horizontals ++ diagonals)
    .groupBy(identity)
    .filter { case (keys, values) => values.size >= 2 }
    .keys
    .size

  println(result2)

}
