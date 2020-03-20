package chapter15

import org.scalatest.{Matchers, PropSpec}
import org.scalatest.prop.PropertyChecks
import chapter5.Stream

class ProcessTest extends PropSpec with PropertyChecks with Matchers {

  property("liftOne a function that applies itself once to a stream to a Process") {
    forAll{ l: List[String] =>
      whenever (l.size > 0) {
        val p = Process.liftOne((s: String) => s +"done")
        val actual = p(Stream(l:_*)).toList
        actual should be (List(l(0)+"done"))
      }
    }
  }

  property("lift a function that adds appends a value to a stream of Strings to a Process") {
    forAll{ l: List[String] =>
      val p = Process.lift((s: String) => s +"done")
      val actual = p(Stream(l:_*)).toList
      actual should be (l.map(s => s +"done"))
    }
  }

  property("lift a function that repeats some value forever  to a Process") {
    val aas = Process.lift((_:Unit) => "a")(Stream.continually(()))
    val actual = aas.take(1200).toList
    actual should be (List.fill(1200)("a"))
  }

  property("filter a stream using a Process") {
    forAll{l: List[Int] =>
      val even = Process.filter((x: Int) => x % 3 == 0)
      val all = Stream(l:_*)

      val actual = even(all)
      actual.toList should be (l.filter(_ % 3 == 0))
    }
  }

  property("sum a stream using a Process") {
   forAll{l: List[Double] =>
     val all = Stream(l:_*)
     val actual = Process.sum(all).toList
     val expected = l.foldLeft(List.empty[Double])((acc, x) => acc match {
       case y :: xs => (x + y) +: acc
       case _ => x +: acc
     }).reverse
     actual.toList should be (expected)
    }
  }
}
