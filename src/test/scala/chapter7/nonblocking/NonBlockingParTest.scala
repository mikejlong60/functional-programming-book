package chapter7.nonblocking

import org.scalatest.prop.PropertyChecks
import org.scalatest.{Matchers, PropSpec}
import org.scalactic.TypeCheckedTripleEquals._ 
import Nonblocking.Par._
import java.util.concurrent._

class NonBlockingParTest extends PropSpec with PropertyChecks with Matchers {
  val executor2 = Executors.newFixedThreadPool(8)

  def sumInParallel2[A](ints: List[Int]): Nonblocking.Par[Int] = {//Give the thread 10 millisecond timeout.
    if (ints.size <= 1) {
      val p = unit(ints.headOption getOrElse 0)
      p
    } else {
      val (l, r) = ints.splitAt(ints.length/2)
      map2(fork(sumInParallel2(l)), fork(sumInParallel2(r)))((x, y) => x + y)
    }
  }

//  property("prove that you don't have a deadlock as in exercise 7.9") {// Thread pool is big enough
//    forAll { xs: List[Int] => {
//    val actual = sumInParallel2(xs)(executor2)
//    val expected = xs.sum
//    actual should be (expected)
//    }
//    }
//  }
  def lawOfFork[A](a: Nonblocking.Par[A])(es: ExecutorService): Boolean = {
    val aa = Nonblocking.Par.run(es)(fork(a))
    val bb = Nonblocking.Par.run(es)(a)
    aa == bb
  }
  property("law of fork") {
    forAll{(x: Int, xs: List[Int]) =>
      val g  = (i: Int) =>  i + x
      lawOfFork(unit(g))(executor2) should be (true)
      lawOfFork(unit(x))(executor2) should be (true)
      lawOfFork(unit(xs))(executor2) should be (true)
    }
  }
}

