package chapter7

import java.util.concurrent._
import language.implicitConversions

object Par {

  type Par[A] = ExecutorService => Future[A]

  def unit[A](a:  => A): Par[A] = (es: ExecutorService) => UnitFuture(a)

  private case class UnitFuture[A](get: A) extends Future[A] {
    def isDone = true
    def get(timeout: Long, units: TimeUnit) = get
    def isCancelled = false
    def cancel(evenIfRunning: Boolean): Boolean = false
  }

  def map2[A, B, C](a: Par[A], b: Par[B])(f: (A, B) => C): Par[C] =
    (es: ExecutorService) => {
      val af = a(es)
      val bf = b(es)
      UnitFuture(f(af.get, bf.get))
    }

  def fork[A](a: => Par[A]): Par[A] =
        es => es.submit(new Callable[A] {
          def call = a(es).get
        })
    

  def sumInParallelNot(ints: List[Int]): Int = {
    if (ints.size <= 1) ints.headOption getOrElse 0
    else {
      val (l, r) = ints.splitAt(ints.length/2)
      sumInParallelNot(l) + sumInParallelNot(r)
    }
  }

//  def sumInParallel[A](ints: List[Int], myPar: Par[Int]): Int = {
//    if (ints.size <= 1) {
//      val p = myPar.unit(ints.headOption getOrElse 0)
//      12//myPar.get(p)// This is dumb
//    } else {
//      val (l, r) = ints.splitAt(ints.length/2)
//      sumInParallel(l, myPar) + sumInParallel(r, myPar)
//    }
//  }


}

//case class MyPar[String](x: String) extends Par[String] {
////  def unit(a: => String): MyPar[String] = MyPar(a)
//  def get(a: Par[String]):String =  ???
//}
