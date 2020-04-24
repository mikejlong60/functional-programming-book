package chapter14

import org.scalacheck._
import Prop.{forAll, propBoolean}

object QuicksortTest extends Properties("Quicksort tests") {

  property("test first imperative version from book") =
    forAll {xs: List[Int] =>
      val actual = QuickSort.quickSort(xs)
      actual == xs.sorted
    }
}
