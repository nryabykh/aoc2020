package com.github.nryabykh
package aoc2020

import scala.util.{ Try, Success }

/**
 * byr (Birth Year) - four digits; at least 1920 and at most 2002.
 * iyr (Issue Year) - four digits; at least 2010 and at most 2020.
 * eyr (Expiration Year) - four digits; at least 2020 and at most 2030.
 * hgt (Height) - a number followed by either cm or in:
 * If cm, the number must be at least 150 and at most 193.
 * If in, the number must be at least 59 and at most 76.
 * hcl (Hair Color) - a # followed by exactly six characters 0-9 or a-f.
 * ecl (Eye Color) - exactly one of: amb blu brn gry grn hzl oth.
 * pid (Passport ID) - a nine-digit number, including leading zeroes.
 * cid (Country ID) - ignored, missing or not.
 */

class Day04PassportProcessor(input: String, debug: Boolean = false) {
  private val passports = input.split("\n\n").map(_.replaceAll("\n", " ")).toList

  def getCorrect(required: List[String], optional: List[String]): Int = {
    passports.count(x => {
      val fields = x.split(" ").map(y => y.split(":").headOption.getOrElse(""))
      required.forall(fields.contains(_))
    })
  }

  def isByrCorrect(byr: String): Boolean = Try(byr.toInt) match {
      case Success(y) if (y >= 1920) && (y <= 2002) => true
      case _ => false
  }

  def isIyrCorrect(iyr: String): Boolean = Try(iyr.toInt) match {
    case Success(y) if ((y >= 2010) && (y <= 2020)) => true
    case _ => false
  }

  def isEyrCorrect(eyr: String): Boolean = Try(eyr.toInt) match {
    case Success(y) if ((y >= 2020) && (y <= 2030)) => true
    case _ => false
  }

  def isHgtCorrect(hgt: String): Boolean = {
    val pattern = """(\d+)(in|cm)""".r
    hgt match {
      case pattern(h, "cm") if (h.toInt >= 150) && (h.toInt <= 193) => true
      case pattern(h, "in") if (h.toInt >= 59) && (h.toInt <= 76) => true
      case _ => false
    }
  }

  def isHclCorrect(hcl: String): Boolean = {
    val pattern = """#([a-fA-F0-9]{6})""".r
    hcl match {
      case pattern(_) => true
      case _ => false
    }
  }

  def isEclCorrect(ecl: String): Boolean = {
    val correctColors = "amb blu brn gry grn hzl oth".split(" ").toList
    correctColors.contains(ecl)
  }

  def isPidCorrect(pid: String): Boolean = {
    val pattern = """(\d{9})""".r
    pid match {
      case pattern(_) => true
      case _ => false
    }
  }

  private def isCorrect(field: String): String => Boolean = {
    field match {
      case "byr" => isByrCorrect
      case "eyr" => isEyrCorrect
      case "iyr" => isIyrCorrect
      case "hgt" => isHgtCorrect
      case "hcl" => isHclCorrect
      case "ecl" => isEclCorrect
      case "pid" => isPidCorrect
      case _ => _ => true
    }
  }

  def getCorrectWithRules(required: List[String], optional: List[String]): Int = {
    def checkPassport(passport: String): Boolean = {
      val fields: List[Field] = passport
        .split(" ").toList
        .map(f =>
          f.split(":").toList match {
            case h :: t :: _ => Field(h.toString, t.toString)
        })
      val fieldNames = fields.map(_.name)
      val containsAllRequiredFields = required.forall(fieldNames.contains(_))
      val allFieldsAreCorrect = fields.forall(f => isCorrect(f.name)(f.value))
      containsAllRequiredFields && allFieldsAreCorrect
    }

    passports.count(checkPassport)
  }
}

case class Field(name: String, value: String)
