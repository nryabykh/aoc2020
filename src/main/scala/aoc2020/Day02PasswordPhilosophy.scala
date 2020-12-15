package com.github.nryabykh
package aoc2020

import scala.util.matching.Regex

abstract class Checker(input: String, debug: Boolean = false) {
  val pattern: Regex = """(\d+)\-(\d+)\s([a-z]):\s(.*?)$""".r
  val (min, max, char, password) = input match {
    case pattern(_min, _max, _char, _password) => (_min.toInt, _max.toInt, _char, _password)
  }
  if (debug) println(min, max, char, password)

  def isCorrect: Boolean
  if (debug) println(isCorrect)
}

class SimpleChecker(input: String, debug: Boolean = false) extends Checker(input, debug) {
  override val isCorrect: Boolean = {
    val cnt = password.count(_.toString == char)
    (cnt >= min) && (cnt <= max)
  }
}

class TrickyChecker(input: String, debug: Boolean = false) extends Checker(input, debug) {
  override val isCorrect: Boolean = {
    (password(min - 1).toString == char) ^ (password(max - 1).toString == char)
  }
}

case class Day02PasswordPhilosophy(input: List[String], checker: String, debug: Boolean = false) {
  val cnt: Int = input.count(x => checker match {
    case "simple" => new SimpleChecker(x, debug).isCorrect
    case "tricky" => new TrickyChecker(x, debug).isCorrect
  })
}
