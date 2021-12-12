package databrewers
package adventofcode

import scala.io.Source

object Day10 extends App {

  val lines: List[String] = Source
    .fromResource(s"Day10.txt")
    .getLines()
    .toList

  lines foreach println

  val squareBracketRegex = "\\[([^\\[\\]+])".r

  "[({(<(())[]>[[{[]{<()<>>" match {
      case squareBracketRegex(a) => println("lol!")
  }


}
