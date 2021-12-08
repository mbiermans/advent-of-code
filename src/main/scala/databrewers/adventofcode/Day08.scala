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



  val result2: List[String] = inputs.map{
      case List(left, right) => 
        val lookupMap = findNumbers(left.split(" ").toList.map(_.sorted))
        val numbers = right.split(" ").map( str => lookupMap(str.sorted)).mkString("")
        numbers
  }

  // lengths:
    /** 0 -> 6 ok
     *  1 -> 2 ok
     *  2 -> 5 ok
     *  3 -> 5 ok
     *  4 -> 4 ok
     *  5 -> 5 ok
     *  6 -> 6 ok
     *  7 -> 3 ok
     *  8 -> 7 ok
     *  9 -> 6 ok
     */

  def findNumbers(input: List[String]): Map[String, Int] = {

    val one = input.find(_.length == 2).get
    val four = input.find(_.length == 4).get
    val seven = input.find(_.length == 3).get
    val eight = input.find(_.length == 7).get

    val nine = input.find(str => str.length == 6 && str.diff(four).length == 2 && str.diff(seven).length == 3).get
    val six = input.find(str => str.length == 6 && str.diff(eight).length == 0 && seven.diff(str).length == 1).get
    val zero = input.find(str => str.length == 6 && str != six && str != nine).get

    val three = input.find(str => str.length == 5 && seven.diff(str).length == 0).get
    val five = input.find(str => str.length == 5 && nine.diff(str) == one.diff(str)).get
    val two = input.find(str => str.length == 5 && str != three && str != five).get


    Map(
      zero -> 0,
      one -> 1,
      two -> 2,
      three -> 3,
      four -> 4,
      five -> 5,
      six -> 6,
      seven -> 7,
      eight -> 8,
      nine -> 9
    )
  }

  println(result2.map(_.toInt).sum)

}
