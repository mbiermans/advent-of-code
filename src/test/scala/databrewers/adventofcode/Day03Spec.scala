package databrewers
package adventofcode

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class Day03Spec extends AnyFlatSpec with Matchers {

  it should "get the gamma" in {

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

    val expected = "10110"

    val output = Day03.getBitString(input)

    output shouldBe expected
  }

  it should "get the epsilon" in {

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

    val expected = "01001"

    val output = Day03.getBitString(input).map(Day03.flipBits)

    output shouldBe expected
  }

  it should "get the generator" in {
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

    val expected = "10111"

    val output = Day03.filterBasedOnCharIndex(input, 0)

    output shouldBe expected
  }

  it should "get the scrubber" in {
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

    val expected = "01010"

    val output = Day03.filterBasedOnCharIndex(input, 0, Day03.flipBits)

    output shouldBe expected
  }
}
