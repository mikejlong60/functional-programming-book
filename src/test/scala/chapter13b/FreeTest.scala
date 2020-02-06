package chapter13b

import org.scalatest.prop.PropertyChecks
import org.scalatest.{Matchers, PropSpec}
import java.util.concurrent._
import chapter7.nonblocking.Nonblocking.Par

class FreeTest extends PropSpec with PropertyChecks with Matchers {
  val executor = Executors.newFixedThreadPool(60)

  property("Goof around with Async ") {
    forAll{ x: Short =>
      whenever( x < 3000) {  //I get a stack overflow with a list longer than ~3000 elements. I think its in Par because there is not a stack overflow for TailRec which is mostly the same.
        import Free._
   //     val f: Int => Free[Int] = (x: Int) =>  Return(x*10)

   //     val j: (Int) => Free[Int] = List.fill(x)(f).foldLeft(f) {(a, b) => x  => a(x)}
    //    val p: Par[Int] = Run.run(j(x))
    //    val actual =Par.run(executor)(p).get
    //    actual should be (x * 10)
      }
    }
  }
}
