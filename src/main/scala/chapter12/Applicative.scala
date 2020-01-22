package chapter12

trait Applicative[F[_]] extends chapter11.Functor[F] {
  def map2[A,B,C](fa: F[A], fb: F[B])(f: (A,B) => C): F[C]
  def unit[A](a: => A): F[A]

  def map3[A,B,C,D](fa: F[A], fb: F[B], fc: F[C])(f: (A,B,C) => D): F[D] = {
    val fab = map2(fa, fb)((a, b) => (a, b))
    map2(fab, fc)((ab, c) => f(ab._1, ab._2, c))
  }

  def map4[A,B,C,D,E](fa: F[A], fb: F[B], fc: F[C], fd: F[D])(f: (A,B,C,D) => E): F[E] = {
    val fab = map2(fa, fb)((a, b) => (a, b))
    val fcd = map2(fc, fd)((c, d) => (c,d))
    map2(fab, fcd)((ab, cd) => f(ab._1, ab._2, cd._1, cd._2))
  }

  def map5[A,B,C,D,E,G](fa: F[A], fb: F[B], fc: F[C], fd: F[D], fe: F[E])(f: (A,B,C,D,E) => G): F[G] = {
    val fabcd = map4(fa, fb, fc, fd)((a, b, c, d) => (a, b, c, d))
    map2(fabcd, fe)((abcd, e) => f(abcd._1, abcd._2, abcd._3, abcd._4, e))
  }

  def map[A,B](fa: F[A])(f: A => B): F[B] = map2(fa, unit(()))((a, _) => f(a))

  def traverse[A,B](as: List[A])(f: A => F[B]): F[List[B]] = as.foldRight(unit(List[B]()))((a, fbs) => map2(f(a), fbs)(_ :: _))

  def sequence[A](fas: List[F[A]]): F[List[A]] = fas.foldRight(unit(List[A]()))((a, fas) =>  map2(a, fas)(_ :: _))

  def replicateM[A](n: Int, fa: F[A]): F[List[A]] = sequence(List.fill(n)(fa))

  def product[A,B](fa: F[A], fb: F[B]): F[(A, B)] = map2(fa, fb)((a, b) => (a, b))
}

object ApplicativeInstances {
  def option = new Applicative[Option] {
    def map2[A,B,C](fa: Option[A], fb: Option[B])(f: (A,B) => C): Option[C] = fa match {
      case Some(a) => fb.map(b => f(a, b))
      case None => None
    }
    def unit[A](a: => A): Option[A] = Option(a)
  }

  def either[S] = new Applicative[({type f[x] = chapter4.Either[S, x]}) #f] {
    def map2[A,B,C](fa: chapter4.Either[S, A], fb: chapter4.Either[S, B])(f: (A,B) => C): chapter4.Either[S, C] = fa match {
      case chapter4.Right(a) => fb.map(b => f(a, b))
      case l @ chapter4.Left(_) => l
    }
    def unit[A](a: => A): chapter4.Either[S, A] = chapter4.Right(a)
  }


}
