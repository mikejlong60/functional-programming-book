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


  property("Test polymorphic is-sorted function for an Int array") {

    val isOrdered: (Int, Int) => Boolean = (x: Int, y: Int) => x <= y
    forAll { n: Array[Int] =>
      println(s"COMPUTING isSorted(${n.mkString(",")}) == ${isSorted(n)(isOrdered)})}")
      isSorted(Array(1, 2, 3, 4, 5))(isOrdered) should be(true)
      isSorted(Array(1, 2, 3, 4, 1))(isOrdered) should be(false)
      isSorted(Array(-1, 2, 3, 4, 5))(isOrdered) should be(true)
      isSorted(Array(1, 2, 3, 4, 5000))(isOrdered) should be(true)
      isSorted(Array(0))(isOrdered) should be(true)
      isSorted(Array())(isOrdered) should be(true)
      isSorted(Array(12, -1, 2, 3, 4, 5))(isOrdered) should be(false)
    }
  }

  property("Test polymorphic is-sorted function for a String array") {

    val isOrdered: (String, String) => Boolean = (x: String, y: String) => x <= y
    forAll { n: Array[String] =>
      println(s"COMPUTING isSorted(${n.mkString(",")}) == ${isSorted(n)(isOrdered)})}")
      isSorted(Array("1", "2", "3", "4", "5"))(isOrdered) should be(true)
      isSorted(Array("1", "2", "3", "4", "1"))(isOrdered) should be(false)
      isSorted(Array("-1", "2", "3", "4", "5"))(isOrdered) should be(true)
      isSorted(Array("1", "2", "3", "4", "5000"))(isOrdered) should be(true)
      isSorted(Array("0"))(isOrdered) should be(true)
      isSorted(Array())(isOrdered) should be(true)
      isSorted(Array("12", "-1", "2", "3", "4", "5"))(isOrdered) should be(false)

    }
  }
}

