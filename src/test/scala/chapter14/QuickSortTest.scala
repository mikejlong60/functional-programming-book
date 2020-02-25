package chapter14

import org.scalatest.prop.PropertyChecks
import org.scalatest.{Matchers, PropSpec}

class QuickSortTest extends PropSpec with PropertyChecks with Matchers {

  property("test first imperative version from book") {
    forAll {xs: List[Int] =>
      val actual = QuickSort.quickSort(xs)
      actual should be (xs.sorted)
    }
  }
}
