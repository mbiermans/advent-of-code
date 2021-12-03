package databrewers
package adventofcode

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class Day03Spec extends AnyFlatSpec with Matchers {

  it should "get the gamma and epsilon" in {

    val input = List(
      "00100",
      "11110",
      "10110",
      "10111",
      "10101",
      "01111",
      "00111",
      "11100",
      "10000",
      "11001",
      "00010",
      "01010"
    )

    val expected = ("10110", "01001")

    val output = Day03.getGammaEpsilon(input)

    output shouldBe expected

  }

  it should "get the oxygen and co2" in {

    val input = List(
      "00100",
      "11110",
      "10110",
      "10111",
      "10101",
      "01111",
      "00111",
      "11100",
      "10000",
      "11001",
      "00010",
      "01010"
    )

    val expected = ("10111", "01010")

    val output = Day03.getOxygenCo2(input)

    output shouldBe expected

  }
}
