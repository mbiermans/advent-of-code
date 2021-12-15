package databrewers
package adventofcode

import scala.io.Source
import scala.collection.mutable

object Day14 extends App {

  val input = Source
    .fromResource("Day14.txt")
    .getLines
    .toList
    .filter(_.nonEmpty)

  val polymer = input.head
  val mapping = input.tail
    .map(_.split(" -> "))
    .map(arr => arr(0) -> arr(1))
    .toMap

  val becomeMappings: Map[String, List[String]] = mapping.map { case (key, value) =>
    key -> List(key(0) + value, value + key(1))
  }

  val charCounter: Map[String, Long] = polymer.grouped(1).toList.groupBy(identity).map{ case (char, list) => char -> list.size.toLong}

  def makePolymers(polymer: Map[String, Long], charMapper: Map[String, String], charCounter: mutable.Map[String, Long], steps: Int = 0): mutable.Map[String, Long] = {
      if(steps == 0) charCounter
      else {
          val newPolymer: Map[String, Long] = (
            for {
                (pol, count) <- polymer.toList
                char         =  charMapper(pol)
                oldCount     =  charCounter.getOrElse(char, 0L)
                _            =  charCounter.update(char, oldCount + count)
                newPol       <- becomeMappings(pol)
            } yield newPol -> count
          ).groupBy(_._1).map{case (key, list) => key -> list.map(_._2).sum}

        //   val newCounter = (spawnedChars.toList ++ charCounter.toList).groupBy(_._1).map{ case (pol, sizes) => pol -> sizes.map(_._2).sum}

          makePolymers(newPolymer, charMapper, charCounter, steps - 1)
      }
  }

  val polymerSliding = polymer.sliding(2).toList.groupBy(identity).map{ case (key, value) => key -> value.size.toLong}
  val result1Map: List[(String, Long)] = makePolymers(polymerSliding, mapping, mutable.Map.from(charCounter), 10)
    .toList.sortBy(_._2)

  val result1 = result1Map.last._2 - result1Map.head._2

  assert(result1 == 2621)

  val result2Map: List[(String, Long)] = makePolymers(polymerSliding, mapping, mutable.Map.from(charCounter), 40)
    .toList.sortBy(_._2)

  val result2 = result2Map.last._2 - result2Map.head._2

  println(result2)

}
