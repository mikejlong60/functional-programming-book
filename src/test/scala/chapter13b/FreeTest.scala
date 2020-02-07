package chapter13b

import org.scalatest.prop.PropertyChecks
import org.scalatest.{Matchers, PropSpec}
import java.util.concurrent._
import chapter7.nonblocking.Nonblocking.Par

class FreeTest extends PropSpec with PropertyChecks with Matchers {
  //val executor = Executors.newFixedThreadPool(60)

  property("Goof around with Free ") {
    forAll{ x: Short =>
      whenever( x < 3000) {  //I get a stack overflow with a list longer than ~3000 elements. I think its in Par because there is not a stack overflow for TailRec which is mostly the same.
        import Free._

        val streamMon: chapter11.Monad[chapter5.Stream] = chapter11.Monad.streamMonad

        val o = Stream.cons(1, Stream.cons(2, Stream.cons(1, Stream.empty)))
 
        val f  = (x: Int) => Free.Return(x)

        //val j  = List.fill(x)(f).foldLeft(f) {(a: Function1[Int, TailRec [Int]], b: Function1[Int, TailRec[Int]]) => (x: Int) => TailRec.suspend(a(x).flatMap(b)) }

        //(Free.run(j(x))) should be (x)
  
        //val freeMon = freeMonad[TailRec]//[streamMon]

        //freeMonad.flatMap(TailRec)((f => _)
   //     val f: Int => Free[Int] = (x: Int) =>  Return(x*10)

   //     val j: (Int) => Free[Int] = List.fill(x)(f).foldLeft(f) {(a, b) => x  => a(x)}
    //    val p: Par[Int] = Run.run(j(x))
    //    val actual =Par.run(executor)(p).get
    //    actual should be (x * 10)
      }
    }
  }
}
