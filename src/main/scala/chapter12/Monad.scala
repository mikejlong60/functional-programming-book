package chapter12

trait Monad[F[_]] extends Applicative[F] {

  def flatMap[A,B](fa: F[A])(f: A => F[B]): F[B] = join(map(fa)(f))

  def join[A](ffa: F[F[A]]): F[A] = flatMap(ffa)(fa => fa)

  def compose[A,B,C](f: A => F[B], g: B => F[C]): A => F[C] = a => flatMap(f(a))(g)

  //def map[A, B](fa: F[A])(f: A => B):  F[B] = flatMap(fa)((a:A) => unit(f(a)))

  //def map2[A,B,C](fa: F[A], fb: F[B])(f: (A, B) => C):F[C] //= flatMap(fa)(a => map(fb)(b =>f(a, b)))

}

object MonadInstances {

  def stream = new Monad[chapter5.Stream] {
    def unit[A](a: => A): chapter5.Stream[A] = chapter5.Stream.continually(a)

    override def map2[A, B, C](a: chapter5.Stream[A], b: chapter5.Stream[B])(f: (A,B) => C): chapter5.Stream[C] = chapter5.Stream.zip(a,b).map(f.tupled)
  }

  def validation[S] = new Monad[({type f[x] = Validation[S, x]}) #f] {
    def apply[A,B](fab: Validation[S, A => B])(fa: Validation[S, A]): Validation[S, B] = (fab, fa) match {
      case (Success(f), Success(a)) =>
        val ffab = (a:A, _: Unit) => f(a)
        _map2(fa, unit(()))(ffab)
      case (Success(f), Failure(h, t)) =>
        val ffab = (a:A, _: Unit) => f(a)
        _map2(fa, unit(()))(ffab)
      case (fail @ Failure(h, t), _) => fail
      case (Failure(h, t), Failure(hh,tt)) => Failure(hh, (h +: t) ++ tt)
    }

    override def map2[A, B, C](fa: Validation[S, A], fb: Validation[S, B])(f: (A, B) => C): Validation[S, C] = (fa, fb) match {
      case (Success(a), Success(b)) =>
        val aaf = unit(f.tupled)
        apply(aaf)(unit(a,b))
      case (Success(a), fail @ Failure(h, t)) =>
        val aaf = unit(f.tupled)
        apply(aaf)(fail)
      case (fail @ Failure(h, t), Success(a)) =>
        val aaf = unit(f.tupled)
        apply(aaf)(fail)
      case (Failure(h, t), Failure(hh,tt)) => Failure(hh, (h +: t) ++ tt)
    }

    //Switch back the underscore and non-underscore versions to see the tests pass with the map2 definition based upon map2 versus the one based on apply
    def _map2[A, B, C](fa: Validation[S, A], fb: Validation[S, B])(f: (A, B) => C): Validation[S, C] = (fa, fb) match {
      case (Success(a), Success(b)) => Success(f(a, b))
      case (Success(_), fail @ Failure(h, t)) => fail
      case (fail @ Failure(h, t), Success(_)) => fail
      case (Failure(h, t), Failure(hh,tt)) => Failure(hh, (h +: t) ++ tt)
    }

    def unit[A](a: => A): Validation[S ,A] = Success(a)
  }

  def list = new Monad[List] {
    override def map2[A,B,C](fa: List[A], fb: List[B])(f: (A,B) => C): List[C] = fa zip fb map f.tupled
    def unit[A](a: => A): List[A] = List(a)
  }

  def option = new Monad[Option] {
    override def map2[A,B,C](fa: Option[A], fb: Option[B])(f: (A,B) => C): Option[C] = fa match {
      case Some(a) => fb.map(b => f(a, b))
      case None => None
    }
    def unit[A](a: => A): Option[A] = Option(a)
  }

  def either[S] = new Monad[({type f[x] = chapter4.Either[S, x]}) #f] {
    override def map2[A,B,C](fa: chapter4.Either[S, A], fb: chapter4.Either[S, B])(f: (A,B) => C): chapter4.Either[S, C] = fa match {
      case chapter4.Right(a) => fb.map(b => f(a, b))
      case l @ chapter4.Left(_) => l
    }
    def unit[A](a: => A): chapter4.Either[S, A] = chapter4.Right(a)
  }


}
