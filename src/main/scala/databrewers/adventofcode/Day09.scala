package databrewers
package adventofcode

import scala.io.Source

object Day09 extends App {

  val lines: List[String] = Source
    .fromResource(s"Day09.txt")
    .getLines()
    .toList

  val matrix: Map[(Int, Int), Int] = (
    for {
      i   <- lines.indices
      line = lines(i)
      j   <- line.indices
    } yield (j, i) -> line(j).toString.toInt
  ).toMap

  def getNeighbours(coordinates: (Int, Int), grid: Map[(Int, Int), Int]): List[((Int, Int), Int)] = {
    coordinates match {
      case (x, y) =>
        List(
          grid.get((x - 1) -> y).map(value => ((x - 1) -> y) -> value),
          grid.get((x + 1) -> y).map(value => ((x + 1) -> y) -> value),
          grid.get(x -> (y - 1)).map(value => (x -> (y - 1)) -> value),
          grid.get(x -> (y + 1)).map(value => (x -> (y + 1)) -> value)
        ).flatten
    }
  }

  val lowest = (
    for {
      ((x, y), value) <- matrix.toList
      neighbours       = getNeighbours((x, y), matrix)
    } yield if (neighbours.forall(_._2 > value)) Some((x, y), value) else None
  ).flatten

  val result1 = lowest.map(_._2 + 1).sum
  println(result1)

  def getArea(
      toVisit: List[(Int, Int)],
      grid: Map[(Int, Int), Int],
      area: Int = 0,
      visited: List[(Int, Int)] = List.empty
  ): (Int, List[(Int, Int)]) = {
    toVisit match {
      case Nil          => area -> visited
      case head :: tail => {
        val neighbours = getNeighbours(head, grid)
        val newVisited = (head :: visited).distinct
        val newToVisit = (neighbours.filter(_._2 < 9).map(_._1) ++ tail).distinct.diff(newVisited)
        val newArea    = grid.get(head) match {
          case Some(x) if x < 9 => area + 1
          case _                => area
        }
        getArea(toVisit = newToVisit, grid = grid, area = newArea, visited = newVisited)
      }
    }
  }

  val areas = lowest.foldLeft(List.empty[Int] -> List.empty[(Int, Int)]) { case ((accumArea, accumVisited), (coords, _)) =>
    coords match {
      case _ if accumVisited.contains(coords) => accumArea -> accumVisited
      case _                                  =>
        val (area, visited) = getArea(toVisit = List(coords), grid = matrix, visited = accumVisited)
        (area :: accumArea) -> (accumVisited ++ visited)
    }
  }

  val result2 = areas._1.sorted.reverse.take(3).product

  println(result2)

}
