package com.github.nryabykh
package aoc2020

class Day03TreesCalculator(input: String, debug: Boolean = false) {
  val plain: List[String] = input.split("\n").toList
  val lenRight: Int = plain.headOption.map(_.length).getOrElse(0)
  val lenDown: Int = plain.length

  def cnt(stepRight: Int, stepDown: Int): Int = {
    val stepsDown = 0 until lenDown by stepDown
    val stepsRight = 0 until (lenDown * stepRight) by stepRight
    stepsDown.zip(stepsRight).count {
      case(posY, posX) => plain(posY)(posX % lenRight).toString == "#"
    }
  }

  def cnt(steps: Seq[(Int, Int)]): Seq[Int] = {
    steps.map{
      case (stepRight, stepDown) => cnt(stepRight, stepDown)
    }
  }

  def getProduct(steps: Seq[(Int, Int)]): Long = {
    cnt(steps).foldLeft(1L) {
      case (acc, item) => acc * item.toLong
    }
  }
}
