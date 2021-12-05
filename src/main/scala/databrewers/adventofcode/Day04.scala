package databrewers
package adventofcode

import scala.io._
import scala.collection.BitSet
import scala.util.matching.Regex

object Day04 extends App {

  val inputs: List[String] = Source.fromResource("Day04.txt").getLines().toList

  val drawnNumbers: List[Int] = inputs.head.split(",").map(_.toInt).toList

  val matrices = inputs.filter(_.nonEmpty).drop(1).map(_.split(" ").toList.filter(_.nonEmpty).map(_.toInt))

  val grids = for {
    index <- matrices.indices by 5
    horizontals = (index to index + 4).map(y => matrices(y))
    verticals = (0 to 4).map(indexY => (0 to 4).map(horizontals.apply(_)(indexY)))
  } yield Grid(
    rows = horizontals,
    columns = verticals,
    allNums = horizontals.flatten.toList
  )

  def answer1(
      grids: Seq[Grid],
      numbers: List[Int],
      input: List[Int] = List.empty
  ): Option[Int] = {

    numbers match {
      case head :: tail =>
        val drawn = head :: input
        val winner = grids.filter(_.hasWon(drawn))
        if (winner.nonEmpty) {
          winner.headOption.map(_.result(drawn))
        } else {
          answer1(grids, tail, drawn)
        }
      case Nil => None
    }

  }

  def answer2(
      grids: Seq[Grid],
      numbers: List[Int],
      input: List[Int] = List.empty
  ): Option[Int] = {
    numbers match {
      case head :: tail =>
            val drawn = head :: input
            val winners = grids.filter(_.hasWon(drawn))

            if(winners.size == grids.size) {
                grids.headOption.map(_.result(drawn))
            } else {
                val rest = grids.diff(winners)
                answer2(rest, tail, drawn)
            }
      case Nil => None
    }
  }

  val result1 = answer1(grids, drawnNumbers)
  result1.foreach(println)

  val result2 = answer2(grids, drawnNumbers)
  result2.foreach(println)

}

case class Grid(
    rows: Seq[Seq[Int]],
    columns: Seq[Seq[Int]],
    allNums: Seq[Int]
) {

  def hasWon(input: List[Int]): Boolean = {
    rows.exists(row => row.diff(input).isEmpty) ||
    columns.exists(column => column.diff(input).isEmpty)
  }

  def result(input: List[Int]): Int = {
    allNums.diff(input).sum * input.head
  }

}

case class Nums(values: Seq[Int]) {}
