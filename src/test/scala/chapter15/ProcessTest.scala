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

  property("make a Process that drops the first n elements from a Stream") {
    forAll{l: List[Double] =>
      whenever (l.size > 10) {
        val drop = l.size - 5
        val all: Stream[Double] = Stream(l:_*)
        val p: Process[Double, Double] = Process.drop(drop)
        val actual = p(all)
        actual.toList should be (l.drop(drop))
      }
    }
  }

  property("make a Process that takes elements from a Stream as long as the given predicate remains true") {
    forAll {l: List[Double] => 
      val takeP = (x: Double) => x > 12.0
      val all: Stream[Double] = Stream(l:_*)
      val p: Process[Double, Double] = Process.takeWhile(takeP)
      val actual = p(all)
      actual.toList should be (l.takeWhile(takeP))
    }
  }

  property("make a Process that starts adding elements to a Stream as soon as the given predicate fails") {
   forAll {l: List[Int] =>
     val dropP = (x: Int) => x > 12
     val all: Stream[Int] = Stream(l:_*)
     val p: Process[Int, Int] = Process.dropWhile(dropP)
     val actual = p(all)
     actual.toList should be (l.dropWhile(dropP))
   }
  }

  property("make a Process that emits a running count of the number of elements in a Stream") {
    forAll{l: List[String] =>
      val all = Stream(l:_*)
      val actual = Process.count(all).toList
      val expected = 1 to l.size
      actual should be (expected)
    }
  }

  property("make a Process that emits a running mean  of the elements in a Stream") {
    forAll{l: List[Double] =>
      val all = Stream(l:_*)
      val actual = Process.mean(all).toList
     val expected = l.sum/l.size
      if (l.size  == 0) actual should be (empty)
      else if (l.size ==1) actual.head should be (expected)
      else actual(l.size-1) should be (expected)
      actual.size should be (l.size)
    }
  }

}
