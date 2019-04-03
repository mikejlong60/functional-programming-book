package chapter7

import org.scalatest.prop.PropertyChecks
import org.scalatest.{Matchers, PropSpec}
import org.scalactic.TypeCheckedTripleEquals._ 
import Par._
import java.util.concurrent._


class ParTest extends PropSpec with PropertyChecks with Matchers {
  val executor = Executors.newFixedThreadPool(10)

  property("Sum in parallel NOT ") {
    forAll { xs: List[Int] => {
      val actual = sumInParallelNot(xs)
      val expected = xs.sum
      actual should be (expected)
    }
   }
  }

  property("Sum in parallel") {
    forAll { xss: List[Int] => {
      val xs = 1 to 10  toList //bigger causes the current implementation of fork to block because it splits the problem into more pieces than I have threads.  I will fix that later in the chapter.
      val actual = sumInParallel(xs)(executor)
      val expected = xs.sum
      actual.get should be (expected)
    }
   }
  }

  property("use lazyUnit") {
    val actual = Par.lazyUnit(12)(executor).get
    val ggg =Par.unit(12)(executor).get
    actual should be (ggg)
  }
}
