package chapter15

import org.scalatest.{Matchers, PropSpec}
import org.scalatest.prop.PropertyChecks
import chapter5.Stream

class ProcessTest extends PropSpec with PropertyChecks with Matchers {

  property("make a Process that waits for one element and then stops") {
    forAll{ l: List[String] =>
      whenever (l.size > 0) {
        val p = Process.liftOne((s: String) => s +"done")
        val actual = p(Stream(l:_*)).toList
        actual should be (List(l(0)+"done"))
      }
    }
  }

  property("make a Process that appends `done` to all elements of a stream") {
    forAll{ l: List[String] =>
      val p = Process.lift((s: String) => s +"done")
      val actual = p(Stream(l:_*)).toList
      actual should be (l.map(s => s +"done"))
    }
  }

  property("make a Process that repeats some value forever") {
    val aas = Process.lift((_:Unit) => "a")(Stream.continually(()))
    val actual = aas.take(1200).toList
    actual should be (List.fill(1200)("a"))
  }

  property("make a Process that filters a stream using some predicate") {
    forAll{l: List[Int] =>
      val even = Process.filter((x: Int) => x % 3 == 0)
      val all = Stream(l:_*)

      val actual = even(all)
      actual.toList should be (l.filter(_ % 3 == 0))
    }
  }

  property("make a Process that sums a Stream") {
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

  property("make a Process that takes a given number of elements from a Stream") {
    forAll{l: List[Double] =>
      whenever (l.size > 10) {
        val take = l.size - 5
        val all: Stream[Double] = Stream(l:_*)
        val p: Process[Double, Double] = Process.take(take)
        val actual = p(all)
        actual.toList should be (l.take(take))
      }
    }
  }
}
