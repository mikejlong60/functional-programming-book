package chapter7

import java.util.concurrent.{ Callable, CountDownLatch, ExecutorService }
import java.util.concurrent.atomic.AtomicReference

sealed trait ActorFuture[A] {
  private[chapter7] def apply(k: A => Unit): Unit
}

object ActorPar {

  type ActorPar[A] = ExecutorService => ActorFuture[A]
  def run[A](es: ExecutorService)(p: ActorPar[A]): A = {

    val ref = new AtomicReference[A]

    val latch = new CountDownLatch(1)

    p(es) {
      a => ref.set(a)
      latch.countDown
    }

    latch.await

    ref.get
  }

  def unit[A](a: A): ActorPar[A] =
    es => new ActorFuture[A] {
      def apply(cb: A => Unit): Unit = cb(a)
    }

  def fork[A](a: => ActorPar[A]): ActorPar[A] =
    es => new ActorFuture[A] {
      def apply(cb: A => Unit): Unit = eval(es)(a(es)(cb))
    }

  def eval(es: ExecutorService)(r: => Unit): Unit =
    es.submit(new Callable[Unit] { def call = r})


//  def map2[A, B, C](p: ActorPar[A], p2: ActorPar[B])(f: (A, B) => C): ActorPar[C] =
//    es => new ActorFuture[C] {
//      def apply(cb: C => Unit): Unit = {
//        var ar: Option[A] = None
 //       var br: Option[B] = None

//        def combiner = ActorPar[A, B, C] (es) {
//          case Left(a) => br match {
//            case None => ar = Some(a)
//            case Some(b) => eval(es)(cb(f(a, b)))
//          }
//          case Right(b) => ar match {
//            case None => br = Some(b)
//            case Some(a) => eval(es)(cb(f(a, b)))
//          }
//        //}

//        p(es)(a => combiner ! Left(a))
//        p2(es)(b => combiner ! Right(b))
//      }
//    }

}

