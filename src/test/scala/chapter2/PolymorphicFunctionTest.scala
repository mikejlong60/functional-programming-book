package chapter2

import org.scalatest.prop.PropertyChecks
import org.scalatest.{Matchers, PropSpec}

class PolymorphicFunctionTest extends PropSpec with PropertyChecks with Matchers {

  def isSorted[A](as: Array[A])(ordered: (A, A) => Boolean): Boolean = {

    @annotation.tailrec
    def internal(list: Array[A], lastHead: A): Boolean = {
      if (list.size == 1) ordered(lastHead, list(0))
      else if (ordered(lastHead, list(0))) internal(list.tail, list(0))
      else false
    }

    if (as.size <= 1) true
    else internal(as.tail, as.head)
  }

  def assertSorting[A](n: Array[A])(isOrdered: (A, A) => Boolean) = {
    val sorted = n.sortWith(isOrdered) //This uses an internal Scala library function
    val wasAlreadySorted = isSorted(n)(isOrdered) //This uses my polymorphic, tail-recursive function above.
    if (wasAlreadySorted) sorted shouldBe (n)
    else sorted shouldNot be(n)
  }

  property("Test polymorphic is-sorted function for an Int array") {
    val isOrdered: (Int, Int) => Boolean = (x: Int, y: Int) => x <= y
    forAll { n: Array[Int] =>
      assertSorting(n)(isOrdered)
    }
  }

  property("Test polymorphic is-sorted function for a String array") {
    val isOrdered: (String, String) => Boolean = (x: String, y: String) => x <= y
    forAll { n: Array[String] =>
      assertSorting(n)(isOrdered)
    }
  }

  property("Test polymorphic is-sorted function for a Long array") {
    val isOrdered: (Long, Long) => Boolean = (x: Long, y: Long) => x <= y
    forAll { n: Array[Long] =>
      assertSorting(n)(isOrdered)
    }
  }

  property("Test polymorphic is-sorted function for a Float array") {
    val isOrdered: (Float, Float) => Boolean = (x: Float, y: Float) => x <= y
    forAll { n: Array[Float] =>
      assertSorting(n)(isOrdered)
    }
  }

}

