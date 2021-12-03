package databrewers
package adventofcode

import scala.io._
import scala.collection.BitSet

object Day03 extends App {

  def getBitString(list: List[String]) = {
    val pivoted = list match {
      case head :: _ =>
        for {
          indexX <- head.indices
        } yield list.foldLeft("") { case (accum, next) => accum.appended(next.charAt(indexX)) }
      case Nil => Seq.empty[String]
    }
    pivoted
      .map { case str =>
        if (str.count(char => char == '1') >= str.count(char => char == '0')) "1" else "0"
      }
      .mkString("")
  }

  def flipBits: Char => Char = (c: Char) =>
    c match {
      case '0' => '1'
      case '1' => '0'
    }

  def filterBasedOnCharIndex(list: List[String], index: Int, charMap: Char => Char = identity): String = {

    def _filterBasedOnCharIndex(list: List[String], index: Int, comp: String, charMap: Char => Char): String = {
      if (list.size == 1) {
        list.head
      } else {
        val filteredList = list.filter(str => str(index) == comp(index))
        val bitString = getBitString(filteredList).map(charMap)
        _filterBasedOnCharIndex(filteredList, index + 1, bitString, charMap)
      }
    }

    val bitString = getBitString(list).map(charMap)
    _filterBasedOnCharIndex(list, 0, bitString, charMap)

  }

  val inputs: List[String] = Source.fromResource("Day03.txt").getLines.toList

  val gamma = getBitString(inputs)

  val epsilon = gamma.map(flipBits)

  val generator = filterBasedOnCharIndex(inputs, 0)
  val scrubber = filterBasedOnCharIndex(inputs, 0, flipBits)

  println(s"Result 1: ${Integer.parseInt(gamma, 2) * Integer.parseInt(epsilon, 2)}")
  println(s"Result 2: ${Integer.parseInt(generator, 2) * Integer.parseInt(scrubber, 2)}")

}
