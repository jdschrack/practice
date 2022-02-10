package com.schrack

class ListNode(_x: Int = 0, _next: ListNode = null) {
  var next: ListNode = _next
  var x: Int = _x
}


object MergeAndSortLists extends App {
  // unoptimized
  // runtime 952 ms / 72.3 MB
  def mergeTwoLists(list1: ListNode, list2: ListNode): ListNode = {
    def buildArray(list1: ListNode, list2: ListNode, nums: Array[Int]): Array[Int] = {
      var list1Tmp = list1
      var list2Tmp = list2
      var newArray = nums
      if (list1 != null || list2 != null) {
        if (list1 != null) {
          list1Tmp = list1.next
          newArray = newArray :+ list1.x
        }
        if (list2 != null) {
          list2Tmp = list2.next
          newArray = newArray :+ list2.x
        }
        buildArray(list1Tmp, list2Tmp, newArray)
      } else {
        nums
      }
    }

    def sortArray(nums: Array[Int]): Array[Int] = {
      for (i <- 0 until nums.length - 1) {
        for (j <- 0 until nums.length - i - 1) {
          if (nums(j) > nums(j + 1)) {
            val temp = nums(j)
            nums(j) = nums(j + 1)
            nums(j + 1) = temp
          }
        }
      }
      nums
    }

    def buildListNodeFromArray(nums: Array[Int], listNode: ListNode, i: Int): ListNode = {
      if (i < nums.length) {
        val ln = new ListNode(nums(i), listNode)
        buildListNodeFromArray(nums, ln, i + 1)
      } else {
        listNode
      }
    }

    buildListNodeFromArray(sortArray(buildArray(list1, list2, Array())).reverse, null, 0)
  }

  // optimized1
  // runtime 564 ms / 56.1 MB
  def mergeTwoListsOptimized(list1: ListNode, list2: ListNode): ListNode = {
    var list1Tmp = list1
    var list2Tmp = list2
    var nums: Array[Int] = Array()

    while (list1Tmp != null || list2Tmp != null){
      if (list1Tmp != null) {
        nums = nums :+ list1Tmp.x
        list1Tmp = list1Tmp.next
      }
      if (list2Tmp != null) {
        nums = nums :+ list2Tmp.x
        list2Tmp = list2Tmp.next
      }
    }

    def createListNodes(nums: Array[Int]): ListNode = {
      var listNode: ListNode = null;
      for (item <- 0 until nums.length) {
        if (listNode == null) {
          listNode = new ListNode(nums(item))
        } else {
          listNode = new ListNode(nums(item), listNode)
        }
      }
      listNode
    }

    createListNodes(nums.sortWith(_ < _).reverse)
  }

  /* For testing only */
  def createList(nums: Array[Int]): ListNode = {
    var listNode: ListNode = null;
    for (item <- 0 until nums.length) {
      if (listNode == null) {
        listNode = new ListNode(nums(item))
      } else {
        listNode = new ListNode(nums(item), listNode)
      }
    }
    listNode
  }

  var list1 = createList(Array(1,2,4))
  val list2 = createList(Array(1,3,4))

  val result = mergeTwoListsOptimized(list1, list2)
  println(result)
}
