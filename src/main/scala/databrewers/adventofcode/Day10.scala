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
    case '>' => 1197
    case '}' => 25137
  } 


  def matchBrackets(todo: List[Char], openBrackets: List[Char] = List.empty): Option[Char] = {
    if(todo.isEmpty) None
    else {
      todo match {
        case head :: tail if Set('(', '[', '<', '{').contains(head) =>
          matchBrackets(tail, head :: openBrackets) 
        case head :: tail => 
          val closingBracket = getMatchingOpenBracket(head)
          openBrackets match {
            case h :: t if h == closingBracket => 
              matchBrackets(tail, t)
            case _ => Some(head)
          }
      }
    }

  }

  val result1 = for {
    line    <- lines
    bracket <- matchBrackets(line.toList)
    points  =  getPoints(bracket)
  } yield points

  println(result1.sum)


}
