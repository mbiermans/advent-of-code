package databrewers
package adventofcode

import scala.io.Source

object Day09 extends App {

  val grid: Grid = new Grid(
    Source
      .fromResource(s"Day09.txt")
      .getLines()
      .toList
      .map(_.map(_.toString.toInt).toList)
  )

  case class Pos(x: Int, y: Int)
  class Grid(private val heights: List[List[Int]]) {

    private val xRange: Range = (0 until heights(0).size)
    private val yRange: Range = (0 until heights.size)

    private val height: Pos => Int =
      pos => heights(pos.y)(pos.x)

      
    def heightOption: Pos => Option[Int]     = { pos =>
      val withinRange: Range => Int => Boolean = (range: Range) => range.contains
      val withinRanges: Pos => Boolean         = { pos => withinRange(xRange)(pos.x) && withinRange(yRange)(pos.y) }
      Option(pos).filter(withinRanges).map(height)
    }

    private val neighbours: Pos => List[Pos] = { pos =>
      List(
        (-1, 0),
        (1, 0),
        (0, -1),
        (0, 1)
      ).map { case (nx, ny) => Pos(pos.x + nx, pos.y + ny) }
    }

    def lowPoints: List[Pos] =
      for {
        x               <- xRange.toList
        y               <- yRange
        pos              = Pos(x, y)
        neighbourHeights = neighbours(pos).flatMap(heightOption)
        value            = height(pos)
        if (neighbourHeights.forall(_ > value))
      } yield pos

    def result1: Int =
      lowPoints.map(height).map(_ + 1).sum

    def area(toCheck: List[Pos], accum: List[Pos]) = {
      if(toCheck.isEmpty) accum
      else {

      }
    }

    def result2: Int = 
      
      ???
      

  }

  println(grid.result1)

}
