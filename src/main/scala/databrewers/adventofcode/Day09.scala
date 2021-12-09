package databrewers
package adventofcode

import scala.io.Source

object Day09 extends App {

  val lines: List[String] = Source
    .fromResource(s"Day09.txt")
    .getLines()
    .toList

  val grid: Map[(Int, Int), Int] = (
    for {
      i   <- lines.indices
      line = lines(i)
      j   <- line.indices
    } yield (j, i) -> line(j).toString.toInt
  ).toMap

  val result1 = for {
    ((x, y), value) <- grid
    left             = grid.getOrElse((x - 1) -> y, Int.MaxValue)
    right            = grid.getOrElse((x + 1) -> y, Int.MaxValue)
    up               = grid.getOrElse(x -> (y - 1), Int.MaxValue)
    down             = grid.getOrElse(x -> (y + 1), Int.MaxValue)
  } yield
    if (value < left && value < right && value < up && value < down) value + 1
    else 0

  println(result1.sum)

}
