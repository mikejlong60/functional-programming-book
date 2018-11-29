package chapter3

import org.scalatest.prop.PropertyChecks
import org.scalatest.{Matchers, PropSpec}

class SumProductLengthTest extends PropSpec with PropertyChecks with Matchers {

  property("Test sum function that uses foldLeft") {
    forAll { xs: Array[Int] =>
      val expected = xs.foldLeft(0)((z, x) => z + x)
      val actual = List.foldLeft(List(xs: _*), 0)((z, x) => z + x)
      actual should be(expected)
    }
  }

  property("Test product function that uses foldLeft") {
    forAll { xs: Array[Int] =>
      val expected = xs.foldLeft(0)((z, x) => z * x)
      val actual = List.foldLeft(List(xs: _*), 0)((z, x) => z * x)
      actual should be(expected)
    }
  }

  property("Test length function that uses foldLeft") {
    forAll { xs: Array[Int] =>
      val expected = xs.foldLeft(0)((z, x) => z + 1)
      val actual = List.foldLeft(List(xs: _*), 0)((z, x) => z + 1)
      actual should be(expected)
    }
  }

  property("Understand appended Conses") {
    forAll { (xs1: Array[Int], xs2: Array[Int], xs3: Array[Int], xs4: Array[Int]) =>
      val list1 = List(xs1: _*)
      val list2 = List(xs2: _*)
      val list3 = List(xs3: _*)
      val list4 = List(xs4: _*)
      val app1 = List(list1, list2)
      val app2 = List(list3, list4)
      println("================")
      println(list1)
      println(list2)
      println(list3)
      println(list4)
      println(app1)
      val app3 = List(app1, app2)
      println(app3)

      //val app2 = List(list3, list4)
    }
  }
}

