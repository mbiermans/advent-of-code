package databrewers
package adventofcode

import scala.io.Source

object Day10 extends App {

  val lines: List[String] = Source
    .fromResource(s"Day10.txt")
    .getLines()
    .toList

  def getMatchingOpenBracket(char: Char) = char match {
    case ')' => '('
    case ']' => '['
    case '>' => '<'
    case '}' => '{'
  }

  def getPoints(char: Char): Int = char match {
    case ')' => 3
    case ']' => 57
    case '}' => 1197
    case '>' => 25137
  }

  def matchBrackets(todo: List[Char], openBrackets: List[Char] = List.empty): Option[Char] = {
    (todo, openBrackets) match {
      case (Nil, Nil)                                                    => None
      case (Nil, _)                                                      => None
      case ((head :: tail), _) if Set('(', '[', '{', '<').contains(head) => matchBrackets(tail, head :: openBrackets)
      case ((head :: tail), _)                                           =>
        val matchingBracket = getMatchingOpenBracket(head)
        openBrackets match {
          case (headOpen :: tailOpen) if headOpen == matchingBracket => matchBrackets(tail, tailOpen)
          case _                                                     => Some(head)
        }
    }
  }

  def autoComplete(todo: List[Char], openBrackets: List[Char] = List.empty): List[Char] = {
    (todo, openBrackets) match {
      case (Nil, _)                                                    => openBrackets
      case (head :: tail, _) if Set('(', '[', '{', '<').contains(head) => autoComplete(tail, head :: openBrackets)
      case (head :: tail, Nil)                                         => autoComplete(tail, Nil)
      case (head :: tail, headOpen :: tailOpen)                        => autoComplete(tail, tailOpen)
    }
  }

  def autoCompletePoints(char: Char): Long = char match {
    case '(' => 1L
    case '[' => 2L
    case '{' => 3L
    case '<' => 4L
  }

  val results = for {
    line      <- lines
    bracket    = matchBrackets(line.toList)
    points     = bracket.map(getPoints)
    auto       = if (bracket.isEmpty) autoComplete(line.toList) else List.empty
    autoPoints = auto.map(autoCompletePoints).foldLeft(0L){case (accum, score) => accum * 5L + score}
  } yield (points, autoPoints)

  val result1 = results.flatMap(_._1).sum
  println(result1)

  val result2List = results.filter(_._1.isEmpty).map(_._2).sorted
  val result2 = result2List(result2List.size / 2)
  println(result2)

}
