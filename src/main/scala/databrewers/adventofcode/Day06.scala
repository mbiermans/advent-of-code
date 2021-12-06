package databrewers
package adventofcode

import scala.io._

object Day06 extends App {

  val inputs: Map[Long, Long] = Source
    .fromResource("Day06.txt")
    .getLines()
    .flatMap(_.split(",").map(_.toLong))
    .toList
    .groupBy(identity)
    .map { case (key, value) => key -> value.size.toLong }

  def spawnFish(fishMap: Map[Long, Long], daysLeft: Int): Map[Long, Long] = {
    if (daysLeft == 0) fishMap
    else {

      val newSpawn = (
        for {
          index <- (8L to 0L by -1L)
          fishes <- fishMap.get(index)
        } yield {
          if (index == 0L) List(6L -> fishes, 8L -> fishes)
          else List(index - 1 -> fishes)
        }
      ).toList.flatten.groupBy(_._1).map { case (day, total) => day -> total.map(_._2).sum }

      spawnFish(newSpawn, daysLeft - 1)
    }
  }

  val result1 = spawnFish(inputs, 80)
  println(result1.values.sum)

  val result2 = spawnFish(inputs, 256)
  println(result2.values.sum)

}
