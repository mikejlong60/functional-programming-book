package chapter13b

import org.scalatest.prop.PropertyChecks
import org.scalatest.{Matchers, PropSpec}
import java.util.concurrent._
import chapter7.nonblocking.Nonblocking.Par

class AsyncTest extends PropSpec with PropertyChecks with Matchers {
  val executor = Executors.newFixedThreadPool(60)

  property("Goof around with Async ") {
    forAll{ xx: Short =>
      import Async._

      val x = 3000

      val f: Int => Async[Int] = (x: Int) => Return(x*10)

      val j: (Int) => Async[Int] = List.fill(x)(f).foldLeft(f) {(a, b) => x  => a(x)}
      val p: Par[Int] = Async.run(j(x))
      val actual =Par.run(executor)(p).get
      actual should be (x * 10)
    }
  }
}
