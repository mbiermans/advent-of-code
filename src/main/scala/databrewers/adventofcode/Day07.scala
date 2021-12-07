package databrewers
package adventofcode

import scala.io._
import java.math.BigInteger

object Day07 extends App {

  val inputs: List[Long] = Source
    .fromResource("Day07.txt")
    .getLines()
    .flatMap(_.split(",").map(_.toLong))
    .toList

  val distances = inputs.flatMap { number =>
    for {
      index    <- 0L to inputs.max
      distance = Math.abs(number - index)
    } yield index -> distance
  }

  val result1 = distances
    .groupBy(_._1)
    .map { case (key, list) =>
      key -> list.map(_._2).sum
    }
    .toList
    .sortBy(_._2)
    .head

  println(result1)

  val distances2: List[(Long, Long)] = inputs.flatMap{
    number =>
      for {
        index    <- (0L to inputs.max)
        position = Math.abs(number - index) 
        distance = (0L until position).foldLeft(0L -> 1L){case ((accum, step), _) => (accum + step) -> (step + 1L) }
      } yield index -> distance._1
  }

  val result2 = distances2.groupBy(_._1)
    .map { case (key, list) =>
      key -> list.map(_._2).sum
    }
    .toList
    .sortBy(_._2)
    .head

  println(result2)


}
