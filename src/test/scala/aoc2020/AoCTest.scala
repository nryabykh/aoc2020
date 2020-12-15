package com.github.nryabykh
package aoc2020

import org.scalatest.FlatSpec

class AoCTest extends FlatSpec {
  def convertInputToList(input: String): List[Int] = {
    input.split("\n").map(_.trim.toInt).toList
  }
}
