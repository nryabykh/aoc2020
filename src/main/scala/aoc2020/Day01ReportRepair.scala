package com.github.nryabykh.aoc2020

case class Day01ReportRepair(filename: Option[String] = None) {

  def getTwoMultiplication(input: List[Int]): Int = {
    val second = input.map(x => 2020 - x)
    input.intersect(second) match {
      case h :: t :: _ => h * t
    }
  }

  def getThreeMultiplication(input: List[Int]): Int = {
    val calc = input.map(x => {
      val filtered = input.filter(_ != x)
      val subtracted = filtered.map(y => 2020 - x - y)
      filtered.intersect(subtracted) match {
        case h :: t :: _ => x * h * t
        case _ => -1
      }
    })
    calc.find(_ != -1).getOrElse(-1)
  }
}
