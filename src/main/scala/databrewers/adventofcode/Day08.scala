package databrewers
package adventofcode

import scala.io.Source

object Day08 extends App {

  val inputs: List[List[String]] = Source
    .fromResource(s"Day08.txt")
    .getLines()
    .toList
    .map(_.split("\\|").toList.map(_.trim))

  def count(input: List[List[String]], numChars: Int): Int = {

    input.map { case List(left, right) =>
      right.split(" ").count(_.length == numChars)
    }.sum
  }

  val ones = count(inputs, 2)
  val fours = count(inputs, 4)
  val sevens = count(inputs, 3)
  val eights = count(inputs, 7)

  val result1 = ones + fours + sevens + eights

  println(result1)

  def findNumbers(input: List[String]): Map[String, Int] = {

    val one = input.find(_.length == 2).get
    val four = input.find(_.length == 4).get

    input.map { inp =>
      (inp.length, inp.intersect(one).length, inp.intersect(four).length) match {
        case (6, 2, 3) => inp -> 0
        case (2, 2, 2) => inp -> 1
        case (5, 1, 2) => inp -> 2
        case (5, 2, 3) => inp -> 3
        case (4, 2, 4) => inp -> 4
        case (5, 1, 3) => inp -> 5
        case (6, 1, 3) => inp -> 6
        case (3, _, _) => inp -> 7
        case (7, _, _) => inp -> 8
        case (6, 2, 4) => inp -> 9
      }
    }.toMap
  }

  val result2: List[String] = inputs.map {
    case List(left, right) => {
      val lookupMap = findNumbers(left.split(" ").toList.map(_.sorted))
      right.split(" ").map(str => lookupMap(str.sorted)).mkString("")
    }
  }

  println(result2.map(_.toInt).sum)

}
