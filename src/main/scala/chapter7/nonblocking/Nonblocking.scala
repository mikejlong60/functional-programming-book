package chapter7.nonblocking

import java.util.concurrent.{ Callable, CountDownLatch, ExecutorService }
import java.util.concurrent.atomic.AtomicReference
import language.implicitConversions

object Nonblocking {

  trait Future[+A] {
    private[nonblocking] def apply(k: A => Unit): Unit
  }

  type Par[+A] = ExecutorService => Future[A]

  object Par {

    def async[A](f: (A => Unit) => Unit): Par[A] = es => new Future[A] {
      def apply(k: A => Unit) = f(k)
    }

    def lazyUnit[A](a: => A): Par[A] = fork(unit(a))

    def asyncF[A,B](f: A => B): A => Par[B] = a => lazyUnit(f(a))

    def parMap[A,B](as: List[A])(f: A => B): Par[List[B]] =
      sequence(as.map(asyncF(f)))

    def sequence[A](as: List[Par[A]]): Par[List[A]] =
      map(sequenceBalanced(as.toIndexedSeq))(_.toList)

    def sequenceBalanced[A](as: IndexedSeq[Par[A]]): Par[IndexedSeq[A]] = fork {
      if (as.isEmpty) unit(Vector())
      else if (as.length == 1) map(as.head)(a => Vector(a))
      else {
        val (l,r) = as.splitAt(as.length/2)
        map2(sequenceBalanced(l), sequenceBalanced(r))(_ ++ _)
      }
    }

    def map[A,B](p: Par[A])(f: A => B): Par[B] =
      es => new Future[B] {
        def apply(cb: B => Unit): Unit =
          p(es)(a => eval(es) { cb(f(a)) })
      }

    def run[A](es: ExecutorService)(p: Par[A]): Try[A] = {

      val ref = new AtomicReference[Try[A]]

      val latch = new CountDownLatch(1)

      Try(p(es) {
        a => ref.set(Success(a))
        latch.countDown
      })

      latch.await

      ref.get
    }

    def unit[A](a: A): Par[A] =
      es => new Future[A] {
        def apply(cb: A => Unit): Unit = cb(a)
      }

    def fork[A](a: => Par[A]): Par[A] =
      es => new Future[A] {
        def apply(cb: A => Unit): Unit = eval(es)(a(es)(cb))
      }

    def eval(es: ExecutorService)(r: => Unit): Unit =
      es.submit(new Callable[Unit] { def call = r})


    def map2[A, B, C](p: Par[A], p2: Par[B])(f: (A, B) => C): Par[C] =
      es => new Future[C] {
        def apply(cb: C => Unit): Unit = {
          var ar: Option[A] = None
          var br: Option[B] = None

          val combiner = Actor[Either[A, B]] (es) {
            case Left(a) => br match {
              case None => ar = Some(a)
              case Some(b) => eval(es)(cb(f(a, b)))
            }
            case Right(b) => ar match {
              case None => br = Some(b)
              case Some(a) => eval(es)(cb(f(a, b)))
            }
          }

          p(es)(a => combiner ! Left(a))
          p2(es)(b => combiner ! Right(b))
      }
      }

       /*
     * We can implement `choice` as a new primitive.
     *
     * `p(es)(result => ...)` for some `ExecutorService`, `es`, and
     * some `Par`, `p`, is the idiom for running `p`, and registering
     * a callback to be invoked when its result is available. The
     * result will be bound to `result` in the function passed to
     * `p(es)`.
     *
     * If you find this code difficult to follow, you may want to
     * write down the type of each subexpression and follow the types
     * through the implementation. What is the type of `p(es)`? What
     * about `t(es)`? What about `t(es)(cb)`?
     */
    def choice[A](cond: Par[Boolean])(t: Par[A], f: Par[A]): Par[A] =
      es => new Future[A] {
        def apply(cb: A => Unit): Unit =
          cond(es) { b =>
            if (b) eval(es) { t(es)(cb) }
            else eval(es) { f(es)(cb) }
          }
      }

    def choiceUsingChoiceN[A](cond: Par[Boolean])(t: Par[A], f: Par[A]): Par[A] =
      choiceN(map(cond)(b => if (b) 0 else 1))(List(t,  f))

    def choiceN[A](n: Par[Int])(choices: List[Par[A]]): Par[A] =
      es => new Future[A] {
        def apply(cb: A => Unit): Unit =
          n(es) { index => eval(es) {choices(index)(es)(cb)}}
      }
  }
}
