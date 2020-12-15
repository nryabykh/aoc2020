package com.github.nryabykh
package aoc2020

class Day05BinaryBoarder(input: String) {
  val boardingPasses: List[String] = input.split("\n").filter(_ != "").toList

  private def getId(pass: String) = {
    def binToDec(bin: String): Int = {
      val len = bin.length
      bin.toList.zipWithIndex.map {
        case(b, idx) =>
          b.toString.toInt * math.pow(2, len - idx - 1)
      }.sum.toInt
    }

    val rowBinary = pass.slice(0, 7).replaceAll("F", "0").replaceAll("B", "1")
    val colBinary = pass.slice(7, 10).replaceAll("L", "0").replaceAll("R", "1")
    binToDec(rowBinary) * 8 + binToDec(colBinary)
  }

  def getIDs: List[Int] = boardingPasses.map(getId)
  def getMaxId: Int = getIDs.max

  def getYourID: Int = {
    val ids = getIDs
    val min = getIDs.min
    val max = getIDs.max
    (min to max).sum - ids.sum
  }
}
