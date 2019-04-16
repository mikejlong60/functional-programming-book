package chapter7.nonblocking

import org.scalatest.prop.PropertyChecks
import org.scalatest.{Matchers, PropSpec}
import org.scalactic.TypeCheckedTripleEquals._ 
import Nonblocking.Par._
import java.util.concurrent._

class NonBlockingParTest extends PropSpec with PropertyChecks with Matchers {
  val executor = Executors.newFixedThreadPool(5)

  def sumInParallel[A](ints: List[Int])(es: ExecutorService): Nonblocking.Par[Int] = {//Try sequencebalanced
    if (ints.size <= 1) {
      val p = unit(ints.headOption getOrElse 0)
      p
    } else {
      val (l, r) = ints.splitAt(ints.length/2)
      map2(lazyUnit(sumInParallel(l)(es)), lazyUnit(sumInParallel(r)(es)))((x, y) => Nonblocking.Par.run(es)(x) + Nonblocking.Par.run(es)(y))
    }
  }

//  property("prove that you don't have a deadlock as in exercise 7.9") {
//    forAll { xs: List[Int] =>
//      val actual = Nonblocking.Par.run(executor)(sumInParallel(xs)(executor))
//      val expected = xs.sum
//      actual should be (expected)
//    }
//  }

  property("prove that parMap does not deadlock") {
    val xs = 1 to 100 toList

    println("piss1")
    val a = Nonblocking.Par.parMap(xs)(math.sqrt(_))
    val actual = Nonblocking.Par.run(executor)(a)
    println("piss2")
    val expected = xs.map(math.sqrt(_))
    actual should be (expected)
  }

  def lawOfFork[A](a: Nonblocking.Par[A])(es: ExecutorService): Boolean = {
    val aa = Nonblocking.Par.run(es)(fork(a))
    val bb = Nonblocking.Par.run(es)(a)
    aa == bb
  }
  property("law of fork") {
    forAll{(x: Int, xs: List[Int]) =>
      val g  = (i: Int) =>  i + x
      lawOfFork(unit(g))(executor) should be (true)
      lawOfFork(unit(x))(executor) should be (true)
      lawOfFork(unit(xs))(executor) should be (true)
    }
  }
}

