package chapter7.nonblocking

import Nonblocking.Par._
import java.util.concurrent._
import org.scalacheck._
import Prop.{forAll, propBoolean}

object TestThatUsedToDeadlock extends Properties("Non-blocking Par test that used to deadlock") {

  def sumInParallel[A](ints: List[Int])(es: ExecutorService): Nonblocking.Par[Int] = {
    if (ints.size <= 1) {
      val p =  unit(ints.headOption getOrElse 0)
      p
    } else {
      val (l, r) = ints.splitAt(ints.length/2)
       map2(sumInParallel(l)(es), sumInParallel(r)(es))((x, y) => x + y)
    }
  }

  property("prove that you don't have a deadlock as in exercise 7.9") = {
    val executor = Executors.newFixedThreadPool(1)
    forAll { xs: List[Int] =>
      val a = sumInParallel(xs)(executor)
      val actual = Nonblocking.Par.run(executor)(a)
      val expected = xs.sum
      actual.get == expected
    }
  }
}

