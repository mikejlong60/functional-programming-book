package chapter7

import java.util.concurrent._
import language.implicitConversions

object Par {

  type Par[A] = ExecutorService => Future[A]

  def unit[A](a:  A): Par[A] = (es: ExecutorService) => UnitFuture(a)

  def lazyUnit[A](a: => A): Par[A] = fork(unit(a))

  private case class UnitFuture[A](get: A) extends Future[A] {
    def isDone = true
    def get(timeout: Long, units: TimeUnit) = get
    def isCancelled = false
    def cancel(evenIfRunning: Boolean): Boolean = false
  }

  def map2[A, B, C](a: Par[A], b: Par[B], timeoutMillis: Long)(f: (A, B) => C): Par[C] =
    (es: ExecutorService) => {
      val af = a(es)
      val bf = b(es)
      val start = System.currentTimeMillis
      val aff = af.get(timeoutMillis, TimeUnit.MILLISECONDS)
      val bff = bf.get(timeoutMillis - (System.currentTimeMillis - start), TimeUnit.MILLISECONDS)
      UnitFuture(f(aff, bff))
    }

  def fork[A](a: => Par[A]): Par[A] =
        es => es.submit(new Callable[A] {
          def call = a(es).get
        })
    
  def asyncF[A, B](f: A => B): A => Par[B] = ???

  def sumInParallelNot(ints: List[Int]): Int = {
    if (ints.size <= 1) ints.headOption getOrElse 0
    else {
      val (l, r) = ints.splitAt(ints.length/2)
      sumInParallelNot(l) + sumInParallelNot(r)
    }
  }

  def sumInParallel[A](ints: List[Int]): Par[Int] = {
    if (ints.size <= 1) {
      val p = Par.unit(ints.headOption getOrElse 0)
      p
    } else {
      val (l, r) = ints.splitAt(ints.length/2)
      Par.map2(Par.fork(sumInParallel(l)), Par.fork(sumInParallel(r)), 1000)((x, y) => x + y)
    }
  }
}
