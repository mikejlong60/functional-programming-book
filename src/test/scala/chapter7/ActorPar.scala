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


}

