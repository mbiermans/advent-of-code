package databrewers
package adventofcode

import scala.io.Source

object Day12 extends App {

  val input: List[(String, String)] = Source
    .fromResource(s"Day12.txt")
    .getLines()
    .toList
    .map(_.split("-"))
    .map(arr => (arr(0), arr(1)))

  val leftToRight = input
    .groupBy(_._1)
    .map { case (key, value) =>
      key -> value.map(_._2)
    }
    .toList

  val rightToLeft = input
    .groupBy(_._2)
    .map { case (key, value) =>
      key -> value.map(_._1)
    }
    .toList

  val directionMap = (leftToRight ++ rightToLeft).groupBy(_._1).map { case (key, value) =>
    key -> value.flatMap(_._2)
  }

  def traverse(
      current: String,
      directions: Map[String, List[String]],
      traversed: List[String] = List.empty,
      double: Option[String] = None
  ): List[List[String]] =
    if (current == "end") List(current :: traversed)
    else if (current.forall(_.isLower) && traversed.filter(_ == current).size == 1 && !double.contains(current)) List.empty
    else if (double.contains(current) && traversed.filter(_ == current).size == 2) List.empty
    else {
      val toTraverse = directions(current)
      (
        for {
          next <- toTraverse
        } yield traverse(next, directions, current :: traversed, double)
      ).flatten
    }

  val result = traverse("start", directionMap)
  println(result.size)

  val result2 =
    (for {
      smallCave <- Option.empty[String] :: directionMap.keys
                     .filter(str => str.forall(_.isLower) && !Set("start", "end").contains(str))
                     .map(Option.apply)
                     .toList
    } yield traverse("start", directionMap, List.empty, smallCave)).flatten

  println(result2.distinct.size)

}
