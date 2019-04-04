package chapter7

import org.scalatest.prop.PropertyChecks
import org.scalatest.{Matchers, PropSpec}
import org.scalactic.TypeCheckedTripleEquals._ 
import Par._
import java.util.concurrent._


class ParTest extends PropSpec with PropertyChecks with Matchers {
  val executor = Executors.newFixedThreadPool(2000)

  property("Sum in parallel NOT ") {
    forAll { xs: List[Int] => {
      val actual = sumInParallelNot(xs)
      val expected = xs.sum
      actual should be (expected)
    }
   }
  }

  property("Sum in parallel") {
    forAll { xs: List[Int] => {
      //val xs = 1 to 10  toList //Note the huge size of the thread pool. I did this because I wanted to the test to pass most of the time because Scalacheck does not usually make list that exceed 2000 elemewntas. Making this size of the list bigger than the number of threads in the thread pool causes the current implementation of fork to block because it splits the problem into more pieces than I have threads.  I will fix that later in the chapter.
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

  property("use asyncF") {
    val f = (a: Int) => a + 12
    f(36) should be(48) 
    val a1 = Par.asyncF(f)
    val actual =   a1(36)(executor).get
    actual should be (48)
  }

  property("use other asyncF") {
    val f = (a: Int) => a + 12
    f(36) should be(48) 
    val a1 = Par.asyncF2(f)
    val actual =   a1(36)(executor).get
    actual should be (48)
  }

}

