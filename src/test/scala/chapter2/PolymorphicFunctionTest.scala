package chapter2

import org.scalacheck._

class PolymorphicFunctionTest extends Properties("Polymorphic Functions") {
  import Prop.forAll

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
    if (wasAlreadySorted) sorted == (n)
    else sorted != (n)
  }

  property("Test polymorphic is-sorted function for an Int array") =
    forAll { n: Array[Int] =>
      val isOrdered: (Int, Int) => Boolean = (x: Int, y: Int) => x <= y
      assertSorting(n)(isOrdered)
    }


  property("Test polymorphic is-sorted function for a String array")  =
    forAll { n: Array[String] =>
      val isOrdered: (String, String) => Boolean = (x: String, y: String) => x <= y
      assertSorting(n)(isOrdered)
    }


  property("Test polymorphic is-sorted function for a Long array") =
    forAll { n: Array[Long] =>
      val isOrdered: (Long, Long) => Boolean = (x: Long, y: Long) => x <= y
      assertSorting(n)(isOrdered)
    }


  property("Test polymorphic is-sorted function for a Float array") =
    forAll { n: Array[Float] =>
      val isOrdered: (Float, Float) => Boolean = (x: Float, y: Float) => x <= y
      assertSorting(n)(isOrdered)
    }
}

