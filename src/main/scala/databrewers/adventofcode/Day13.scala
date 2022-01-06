package databrewers
package adventofcode

import scala.io.Source

object Day13 extends App {

  val input: List[String] = Source.fromResource("Day13.txt").getLines.toList.filter(_.nonEmpty)

  val FoldCommandRegex = "fold along (x|y)=([0-9]+)".r
  val PointsRegex      = "([0-9]+),([0-9]+)".r

  val parsed = input.map {
    case PointsRegex(x, y)                                => Right((x.toInt, y.toInt))
    case FoldCommandRegex(letter, point) if letter == "x" => Left(FoldCommand(x = Some(point.toInt)))
    case FoldCommandRegex(letter, point) if letter == "y" => Left(FoldCommand(y = Some(point.toInt)))
  }

  val commands = parsed.collect { case Left(x) => x }
  val points   = parsed.collect { case Right(x) => x }

  def foldPaper(points: List[(Int, Int)], commands: List[FoldCommand]): List[(Int, Int)] = {
    commands match {
      case Nil          => points
      case head :: tail => {
        head match {
          case FoldCommand(None, Some(foldY)) =>
            val newPoints = points.flatMap {
              case (x, y) if y > foldY => Some((x, foldY - (y - foldY)))
              case (_, y) if y == foldY => None
              case point               => Some(point)
            }.distinct
            foldPaper(newPoints, tail)
          case FoldCommand(Some(foldX), None) =>
            val newPoints = points.flatMap {
              case (x, y) if x > foldX  => Some((foldX - (x - foldX), y))
              case (x, _) if x == foldX => None
              case point                => Some(point)
            }.distinct
            foldPaper(newPoints, tail)
          case _                              => ???
        }
      }
    }
  }

  val result1 = foldPaper(points, commands.take(1))
  

  val foldedPaper = foldPaper(points, commands)
  
  
  val maxX = foldedPaper.maxBy(_._1)._1
  val maxY = foldedPaper.maxBy(_._2)._2

  val grid = for {
      y <- 0 to maxY
      line = for {
          x <- 0 to maxX
      } yield if(foldedPaper.contains((x, y))) "#" else "."
  } yield line.mkString("")

  grid foreach println

}

case class FoldCommand(x: Option[Int] = None, y: Option[Int] = None)