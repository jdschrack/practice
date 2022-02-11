package com.schrack

object SubArraySums extends App { // attempt 1

  def subarraySum(nums: Array[Int], k: Int): Int = {
    var count: Int = 0
    for (i <- 0 until nums.length) {
      var sum: Int = 0
      for (j <- i until nums.length) {
        sum = sum + nums(j)
        if (sum == k) {
          count = count + 1
        }
      }
    }
    count
  }

  def subarraySum2(nums: Array[Int], k: Int): Int = {
    def subArraySumRec(nums: Array[Int], k: Int, i: Int, j: Int, count: Int, sum: Int): Int = {
      if (i < nums.length) {
        if (j < nums.length) {
          val newSum = sum + nums(j)
          var newCount = count;
          if (newSum == k) {
            newCount = count + 1
          }
          subArraySumRec(nums, k, i, j + 1, newCount, newSum)
        } else {
          subArraySumRec(nums, k, i + 1, i + 1, count, 0)
        }
      } else {
        count
      }
    }

    subArraySumRec(nums, k, 0, 0, 0, 0)
  }

  printAndExecute(Array(1, 1, 1), 2)
  printAndExecute(Array(1, 2, 3), 3)
  printAndExecute(Array(-1, -1, 1), 0)
  printAndExecute(Array(1, -1, 0), 0)
  printAndExecute(Array(1), 1)
  printAndExecute(Array(1), 0)

  def printAndExecute(nums: Array[Int], sum: Int): Unit = {
    println("======================================")
    println(s"${nums.mkString(",")}, $sum")
    println("======================================")
    var startTimeMillis = System.currentTimeMillis()

    println(subarraySum2(nums, sum))

    var endTimeMillis = System.currentTimeMillis()
    var durationMS = (endTimeMillis - startTimeMillis)
    println(s"Recursive $durationMS ms")

    startTimeMillis = System.currentTimeMillis()

    println(subarraySum(nums, sum))

    endTimeMillis = System.currentTimeMillis()
    durationMS = (endTimeMillis - startTimeMillis)
    println(s"Loop $durationMS ms\n")
  }
}
