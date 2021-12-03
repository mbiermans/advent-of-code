package databrewers
package adventofcode

import scala.io._
import scala.collection.BitSet

object Day03 extends App {

  val One = '1'
  val Zero = '0'

  def flipBit: Char => Char = (c: Char) =>
    c match {
      case '0' => '1'
      case '1' => '0'
    }

  def leastCommon: List[Char] => Char = { list =>
    if (list.count(_ == Zero) < list.count(_ == One))
      Zero
    else
      One
  }

  def mostCommon: List[Char] => Char = { list =>
    if (list.count(_ == Zero) >= list.count(_ == One))
      Zero
    else
      One
  }

  def findBitString(nums: List[List[Char]], index: Int = 0)(commonOf: List[Char] => Char): Int = {
    if (nums.size == 1) {
      Integer.parseInt(nums.head.mkString(""), 2)
    } else {

      val common: Char = commonOf(nums.transpose.apply(index))
      val filtered: List[List[Char]] = nums.filter(bit => bit(index) == common)
      findBitString(filtered, index + 1)(commonOf)

    }

  }

  val inputs: List[List[Char]] = Source.fromResource("Day03.txt").getLines().toList.map(_.toList)

  val gamma = inputs.transpose.map(mostCommon).mkString("")
  val epsilon = inputs.transpose.map(leastCommon).mkString("")

  val oxygen = findBitString(inputs)(mostCommon)
  val co2 = findBitString(inputs)(leastCommon)

  println(s"Result 1: ${Integer.parseInt(gamma, 2) * Integer.parseInt(epsilon, 2)}")
  println(s"Result 2: ${oxygen * co2}")

}
