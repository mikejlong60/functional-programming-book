package chapter14

object QuickSort {
  def quickSort(xs: List[Int]): List[Int] = if (xs.isEmpty) xs else {
    val arr: Array[Int] = xs.toArray
    def swap(x: Int, y: Int) = {
      val tmp = arr(x)
      arr(x) = arr(y)
      arr(y) = tmp
    }

    def partition(l: Int, r: Int, pivot: Int) = {
      val pivotVal: Int = arr(pivot)
      swap(pivot, r)
      var j: Int = l
      for (i <- l until r) if (arr(i) < pivotVal) {
        swap(i, j)
        j += 1
      }
      swap(j, r)
      j
    }

    def qs(l: Int, r: Int): Unit = if (l < r) {
      val pi: Int = partition(l, r, l + (r - l) / 2)
      qs(l, pi - 1)
      qs(pi + 1, r)
    }

    qs(0, arr.length - 1)
    arr.toList
  }
}
