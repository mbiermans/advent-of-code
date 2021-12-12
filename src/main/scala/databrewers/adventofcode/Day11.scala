package databrewers
package adventofcode

import scala.io.Source
import scala.collection.mutable

object Day11 extends App {

  val energy: List[List[Int]] = Source
    .fromResource(s"Day11.txt")
    .getLines()
    .toList
    .map(_.toList.map(_.toString.toInt))

  val gridMap: mutable.Map[(Int, Int), Int] = mutable.Map.from(
    for {
      x <- 0 until energy.head.size
      y <- 0 until energy.size
    } yield (x -> y) -> energy(y)(x)
  )

  class Grid(val grid: mutable.Map[(Int, Int), Int], maxX: Int, maxY: Int) {

    val xRange = 0 until maxX
    val yRange = 0 until maxY

    def isWithinRange(x: Int, y: Int): Boolean =
      xRange.contains(x) && yRange.contains(y)

    private def access(x: Int, y: Int): Int    = grid(x -> y)

    def accessOption(x: Int, y: Int): Option[Int] = {
      grid.get(x -> y)
    }

    def neighbours(x: Int, y: Int): List[(Int, Int)] =
      for {
        nx    <- (-1 to 1).toList
        ny    <- (-1 to 1).toList
        coords = (x + nx, y + ny) if isWithinRange(coords._1, coords._2) && coords != (x, y)
      } yield coords

    def allPositions: List[(Int, Int)] = grid.keys.toList

    def updateLevel(key: (Int, Int)): List[(Int, Int)] = {

      val newLevel = access(key._1, key._2) + 1
      if (newLevel == 10) {
        grid.update(key, newLevel)
        neighbours(key._1, key._2)
      } else {
        grid.update(key, newLevel)
        List.empty
      }

    }

    def resetGrid: Unit = for {
      (key, value) <- grid
      _             = if (value >= 10) grid.update(key, 0)
    } yield ()

  }

  val grid1 = new Grid(gridMap, energy.head.size, energy.size)

  def updateGrid(todo: List[(Int, Int)], flashes: Int = 0, grid: Grid): Int = {
    if (todo.isEmpty) {
      flashes
    } else {
      todo match {
        case head :: next =>
          val extraUpdates = grid.updateLevel(head)
          if (extraUpdates.nonEmpty) {
            updateGrid(next ++ extraUpdates, flashes + 1, grid)
          } else {
            updateGrid(next, flashes, grid)
          }
      }
    }
  }

  val result1 = for {
    i      <- 1 to 100
    flashes = updateGrid(grid1.allPositions, 0, grid1)
    _       = grid1.resetGrid
  } yield flashes

  println(result1.sum)

}
