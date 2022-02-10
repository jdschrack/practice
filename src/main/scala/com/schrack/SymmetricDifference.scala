package com.schrack

object SymmetricDifference extends App {
  /* First lets find the Symmetric Difference between two sets, this is
   * first attempt. Optimization will happen in other functions below.
   * This attempt is not looking anything up online.
   */

  def sym(arr1: Array[Int], arr2: Array[Int]): Array[Int] = {
    def findDiff(arr1: Array[Int], arr2: Array[Int], results: Array[Int], i: Int, j: Int, passSeq: Int): Array[Int] = {
      if (i < arr1.length) {
        if (j < arr2.length) {
          if (arr1(i) == arr2(j)) {
            findDiff(arr1, arr2, results, i, j + 1, passSeq)
          } else if (arr2.contains(arr1(i)) && arr1.contains(arr1(i))) {
            findDiff(arr1, arr2, results, i, j + 1, passSeq)
          } else if (results.contains(arr1(i))) {
            findDiff(arr1, arr2, results, i, j + 1, passSeq)
          } else {
            val newResults = results :+ arr1(i)
            findDiff(arr1, arr2, newResults, i, j + 1, passSeq)
          }
        } else {
          findDiff(arr1, arr2, results, i + 1, 0, passSeq)
        }
      } else if (i == arr1.length && j < arr2.length) {
        findDiff(arr1, arr2, results, i, j + 1, passSeq)
      } else if (passSeq == 1) {
        findDiff(arr2, arr1, results, 0, 0, 2)
      } else {
        results.sortWith(_ < _)
      }
    }
    findDiff(arr1, arr2, Array(), 0, 0, 1)
  }

  var results = sym(Array(1, 2, 3), Array(5, 2, 1, 4))
  println(results.mkString(","))
}
