package databrewers
package adventofcode

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class Day05Spec extends AnyFlatSpec with Matchers {

  import Day05.Coordinates

  it should "build a diagonal to bottom right" in {

    val inputStart = Coordinates(1, 2)
    val inputEnd = Coordinates(4, 5)

    val expected = List(1 -> 2, 2 -> 3, 3 -> 4, 4 -> 5)
      .map(co => Coordinates.apply(co._1, co._2))

    val output = Day05.diagonal(inputStart, inputEnd)

    output shouldBe expected

  }

  it should "build a diagonal to top right" in {

    val inputStart = Coordinates(0, 8)
    val inputEnd = Coordinates(8, 0)

    val expected = List(0 -> 8, 1 -> 7, 2 -> 6, 3 -> 5, 4 -> 4, 5 -> 3, 6 -> 2, 7 -> 1, 8 -> 0)
      .map(co => Coordinates.apply(co._1, co._2))

    val output = Day05.diagonal(inputStart, inputEnd)

    output shouldBe expected

  }

  it should "build a diagonal top right" in {

    val inputStart = Coordinates(102, 380)
    val outputStart = Coordinates(300, 578)

    val expected = (0 to 198)
      .map(x => (102 + x) -> (380 + x))
      .toList
      .map(co => Coordinates.apply(co._1, co._2))

    val output = Day05.diagonal(inputStart, outputStart)

    output shouldBe expected
  }

  it should "build a diagonal bottom right" in {

    val inputStart = Coordinates(102, 380)
    val outputStart = Coordinates(300, 182)

    val expected = (0 to 198)
      .map(x => (102 + x) -> (380 - x))
      .toList
      .map(co => Coordinates.apply(co._1, co._2))

    val output = Day05.diagonal(inputStart, outputStart)

    output shouldBe expected

  }

}
