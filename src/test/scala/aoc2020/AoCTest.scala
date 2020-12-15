package com.github.nryabykh
package aoc2020

import org.scalatest.FlatSpec

abstract class AoCTest extends FlatSpec {
  def sampleInput: String
  def testInput: String
  def convertInputToList(input: String): List[Int] = {
    input.split("\n").map(_.trim.toInt).toList
  }
}
