package chapter11

import scala.{Option => _, None => _,  Right => _, Left => _, Either => _, _}

trait Functor[F[_]] {
  def map[A, B](fa: F[A])(f: A => B): F[B]
  def mapLaw[A](fa: F[A]): Boolean = map(fa)(a => a)  == fa
}

trait Monad[F[_]] extends Functor[F] {
  def unit[A](a: => A): F[A]
  def flatMap[A,B](ma: F[A])(f: A => F[B]): F[B]
  def map[A,B](ma: F[A])(f: A => B): F[B] = flatMap(ma)(a => unit(f(a)))
  def map2[A,B,C](ma: F[A], mb: F[B])(f: (A, B) => C): F[C] = flatMap(ma)(a => map(mb)(b => f(a, b)))
  def sequence[A](lma: List[F[A]]): F[List[A]] = lma.foldRight(unit(List[A]()))((ma, mla) => map2(ma, mla)(_ :: _))
  def traverse[A,B](la: List[A])(f: A => F[B]): F[List[B]] = sequence(la.map(a => f(a)))
  def compose[A,B,C](f: A => F[B], g: B => F[C] ): A => F[C] = a => flatMap(f(a))(g)
  def associativeLaw[A,B](x: F[A])(f: A => F[B])(g: B => F[B]): Boolean  = flatMap(flatMap(x)(f))(g) == flatMap(x)(a => flatMap(f(a))(g))
  def associativeLawUsingKleisli[A,B,C,D](a: A)(f: A => F[B], g: B => F[C], h: C => F[D]): Boolean  = {
    val lf = compose(compose(f, g), h)
    val rf = compose(f, compose(g, h))
    lf(a) == rf(a)
  }
  def identityLawsUsingKleisli[A,B](a: A)(f: A => F[B]) = {
    val li = compose(f, (b: B) => unit(b))
    val ri = compose((a: A) => unit(a), f)
    li(a) == ri(a)
  }
}

object Monad {
   val optionMonad = new Monad[chapter4.Option] {
    def unit[A](a: => A) = chapter4.Some(a)
    def flatMap[A,B](ma: chapter4.Option[A])(f: A => chapter4.Option[B]) = ma flatMap f
  }

  def eitherMonad[S] = new  Monad[({type f[x] = chapter4.Either[S, x]}) #f]  {
    def flatMap[A,B](ma: chapter4.Either[S,A])(f: A => chapter4.Either[S,B]): chapter4.Either[S,B] = ma flatMap f
     
    def unit[A](a: => A): chapter4.Either[S,A] = chapter4.Right(a)
  }

  def stateMonad[S] = new  Monad[({type s[x] = State[S, x]}) #s]  {
    def flatMap[A,B](sa: State[S,A])(f: A => State[S,B]): State[S,B] = sa flatMap f
      
    def unit[A](a: => A): State[S,A] = State(s => (a, s))
  }

}
