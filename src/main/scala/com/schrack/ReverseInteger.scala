package com.schrack

object ReverseInteger extends App {
  def reverse(x: Int): Int = {
    val resultStrg: String = x.toString.replaceAll("-","").toArray.reverse.mkString
    var result: Long = 0
    if (x < 0) {
      result = resultStrg.toLong * -1
    } else {
      result = resultStrg.toLong
    }

    result.toLong match {
      case num if num <= scala.math.pow(-2, 31).toLong => 0
      case num if num >= (scala.math.pow(2, 31) - 1).toLong => 0
      case _ => result.toInt
    }
  }

  println(reverse(-2147483648))
}
