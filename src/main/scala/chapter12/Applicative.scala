package chapter12

trait Applicative[F[_]] extends chapter11.Functor[F] {
 // def apply[A,B](fab: F[A => B])(fa: F[A]): F[B]
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

  def associativeLaw[A,B,C](fa: F[A])(fb: F[B])(fc: F[C]): Boolean  = {
    def assoc[A,B,C](p: (A, (B, C))): ((A,B),C) = p match {
      case (a, (b, c)) => ((a, b), c)
    }
    product(product(fa, fb), fc) == map(product(fa, product(fb, fc)))(assoc)
  }

  def leftAndRightIdentityLaw[A](fa: F[A]): Boolean = {
    val li = map2(unit(()), fa)((_, a) => a)
    val ri = map2(fa, unit(()))((a, _) => a)
    ri == li && ri == fa
  }

  def naturalityOfProductLaw[A, B, A2, B2](fa: F[A], fb: F[A2])(f: A => B)(g: A2 => B2) = {

    def productF(f: A=> B)(g: A2 => B2): (A,  A2) => (B,B2) = (a, a2) => (f(a), g(a2))

    val l = map2(fa, fb)(productF(f)(g))
    val r = product(map(fa)(f), map(fb)(g))
    l == r
  }
}

object ApplicativeInstances {

  def stream = new Applicative[chapter5.Stream] {
    def unit[A](a: => A): chapter5.Stream[A] = chapter5.Stream.continually(a)

    def map2[A, B, C](a: chapter5.Stream[A], b: chapter5.Stream[B])(f: (A,B) => C): chapter5.Stream[C] = chapter5.Stream.zip(a,b).map(f.tupled)
  }

  def validation[S] = new Applicative[({type f[x] = Validation[S, x]}) #f] {
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

    def map2[A, B, C](fa: Validation[S, A], fb: Validation[S, B])(f: (A, B) => C): Validation[S, C] = (fa, fb) match {
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

    //Switch back the ugnderscore and non-underscore versions to see the tests pass with the map2 definition based upon map2 versus the one based on apply
    def _map2[A, B, C](fa: Validation[S, A], fb: Validation[S, B])(f: (A, B) => C): Validation[S, C] = (fa, fb) match {
      case (Success(a), Success(b)) => Success(f(a, b))
      case (Success(_), fail @ Failure(h, t)) => fail
      case (fail @ Failure(h, t), Success(_)) => fail
      case (Failure(h, t), Failure(hh,tt)) => Failure(hh, (h +: t) ++ tt)
    }

    def unit[A](a: => A): Validation[S ,A] = Success(a)
  }

  def list = new Applicative[List] {
    def map2[A,B,C](fa: List[A], fb: List[B])(f: (A,B) => C): List[C] = fa zip fb map f.tupled    
    def unit[A](a: => A): List[A] = List(a)
  }

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
