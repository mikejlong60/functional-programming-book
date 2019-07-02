package chapter7

import java.util.concurrent._
import language.implicitConversions

object Par {

  type Par[A] = ExecutorService => Future[A]

  def unit[A](a:  A): Par[A] = (es: ExecutorService) => UnitFuture(a)

  def lazyUnit[A](a: => A): Par[A] = fork(unit(a))

  def id[A](a: A): A = a

  def lawOfMap[A](a: Par[A])(es: ExecutorService): Boolean =  map(a)(id)(es).get == a(es).get

  def lawOfFork[A](a: Par[A])(es: ExecutorService): Boolean =  fork(a)(es).get == a(es).get
  
  private case class UnitFuture[A](get: A) extends Future[A] {
    def isDone = true
    def get(timeout: Long, units: TimeUnit) = get
    def isCancelled = false
    def cancel(evenIfRunning: Boolean): Boolean = false
  }

  def asyncF[A, B](f: A => B): A => Par[B] = a => {
    (es: ExecutorService) => UnitFuture(f(a))
  }

  def asyncF2[A, B](f: A => B): A => Par[B] = a => lazyUnit(f(a))
  

  def map2[A, B, C](a: Par[A], b: Par[B], timeoutMillis: Long = 1000)(f: (A, B) => C): Par[C] =
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

  def delay[A](fa: => Par[A]): Par[A] = es => fa(es)

  def map[A, B](pa: Par[A])(f: A => B): Par[B] = map2(pa, unit(()), 1000)((a, _) => f(a))

  def sequence[A](ps: List[Par[A]]): Par[List[A]] = {
    (es: ExecutorService) =>
    def fold(z: List[Par[A]]): List[A] = z match {
      case x :: xs => x(es).get :: fold(xs)
      case _ => Nil 
    }

    val as = fold(ps)
    unit(as)(es)
  }

  def parFilter[A](as: List[A])(f: A => Boolean): Par[List[A]] = (es: ExecutorService) => as match {
    case x :: xs => {
      val (l, r) = as.splitAt(as.length/2)
      val split = sequence(List(
        unit(l.filter(xx => f(xx))),
        unit(r.filter(xx => f(xx)))
      ))(es).get.flatten
      unit(split)(es)
    }
    case _ => unit(List.empty[A])(es)
  }

  def parMap[A, B](ps: List[A])(f: A => B): Par[List[B]] = fork {
    val fbs: List[Par[B]] = ps.map(asyncF(f))
    sequence(fbs)
  }

  def sortPar(parList: Par[List[Int]]): Par[List[Int]] = map(parList)( a => a.sorted)


  def sumInParallelNot(ints: List[Int]): Int = {
    if (ints.size <= 1) ints.headOption getOrElse 0
    else {
      val (l, r) = ints.splitAt(ints.length/2)
      sumInParallelNot(l) + sumInParallelNot(r)
    }
  }

  def sumInParallel[A](ints: List[Int]): Par[Int] = {//Give the thread 10 millisecond timeout.
    val timeout = 10
    if (ints.size <= 1) {
      val p = Par.unit(ints.headOption getOrElse 0)
      p
    } else {
      val (l, r) = ints.splitAt(ints.length/2)
      Par.map2(Par.fork(sumInParallel(l)), Par.fork(sumInParallel(r)), timeout)((x, y) => x + y)
    }
  }

  def equal[A](p1: Par[A], p2: Par[A]): Par[Boolean] = Par.map2(p1, p2)(_ == _)


}
