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

  property("Force a deadlock as in exercise 7.9") {
    val executor = Executors.newFixedThreadPool(2)//Making this size of the list bigger than the number of threads in the thread pool causes the current implementation of fork to block because it splits the problem into more pieces than I have threads.  I will fix that later in the chapter.
    val xs  = List(1,2,3,4)
    an [Exception] should be thrownBy  (sumInParallel(xs)(executor).get)
  }

  property("Don't force a deadlock as in exercise 7.9") {// Thread pool is big enough
    val xs = List(1,2,3,4)
    val executor = Executors.newFixedThreadPool(10)
    val actual = sumInParallel(xs)(executor)
    val expected = xs.sum
    actual.get should be (expected)
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

