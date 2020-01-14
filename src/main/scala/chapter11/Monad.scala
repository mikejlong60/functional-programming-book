package chapter11

import scala.{Option => _, None => _,  Right => _, Left => _, Either => _, _}

trait Functor[F[_]] {
  def map[A, B](fa: F[A])(f: A => B): F[B]
  def mapLaw[A](fa: F[A]): Boolean = map(fa)(a => a) == fa
}

trait Monad[F[_]] extends Functor[F] {
  def unit[A](a: => A): F[A]
  def flatMap[A,B](ma: F[A])(f: A => F[B]): F[B]
  def _flatMap[A,B](ma: F[A])(f: A => F[B]): F[B] = {
    val hh = compose((s: Unit) => ma, f)(())
    hh
  }
  def flatMapWithJoin[A,B](ma: F[A])(f: A => F[B]): F[B] ={
    val mma =  map(ma)(s => f(s))
    join(mma)
  }
  def join[A](mma: F[F[A]]): F[A] = flatMap(mma)(ma => flatMap(ma)(a => unit(a)))
  def map[A,B](ma: F[A])(f: A => B): F[B] = flatMap(ma)(a => unit(f(a)))
  def map2[A,B,C](ma: F[A], mb: F[B])(f: (A, B) => C): F[C] = flatMap(ma)(a => map(mb)(b => f(a, b)))
  def map3[A,B,C,D](ma: F[A], mb: F[B], mc: F[C])(f: (A, B, C) => D): F[D] = flatMap(ma)(a => flatMap(mb)(b => map(mc)(c => f(a, b, c))))
  def sequence[A](lma: List[F[A]]): F[List[A]] = lma.foldRight(unit(List[A]()))((ma, mla) => map2(ma, mla)(_ :: _))
  def traverse[A,B](la: List[A])(f: A => F[B]): F[List[B]] = sequence(la.map(a => f(a)))
  def compose[A,B,C](f: A => F[B], g: B => F[C]): A => F[C] = a => flatMap(f(a))(g)
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

  //TODO This is not quite right .  See ListTest
  def replicateM[A](n: Int, ma: F[A]): F[List[A]] = {
    def inner(i: Int, accum: List[F[A]]): List[F[A]] =  {
      if (i > n) accum
      else inner(i + 1, map(ma)(a => a)  :: accum)
    }
    sequence(List.fill(n)(ma))
    //sequence(inner(1, List.empty))
  }

  def filterM[A](ms: List[A])(f: A => F[Boolean]): F[List[A]] = {
    val b: F[List[Boolean]] = traverse(ms)(f)
    val a: F[List[A]] = sequence(ms.map(a => unit(a)))
    val c = map2(b, a)((bools, aas) => aas.zip(bools).filter(pair => pair._2))
    val d = map(c)(e => e.map(f => f._1))
    d
  }
}

object Monad {
  case class Id[A](value: A)

  def idMonad = new Monad[Id] {
    def unit[A](a: => A): Id[A]  = Id(a)
    override def flatMap[A, B](ma: Id[A])(f: A => Id[B]) = f(ma.value)
  }

  def IOMonad = new Monad[IO] {
    def unit[A](a: => A): IO[A] = new IO[A] { def run = a}
    override def flatMap[A, B](ma: IO[A])(f: A => IO[B]): IO[B] = new IO[B] {
      def run = f(ma.run).run
    }
  }

  def streamMonad = new Monad[chapter5.Stream] {
    def unit[A](a: => A): chapter5.Stream[A] = chapter5.Stream[A](a)
    override def flatMap[A, B](ma: chapter5.Stream[A])(f: A => chapter5.Stream[B]): chapter5.Stream[B] = ma flatMap f
  }

  import chapter7.nonblocking.Nonblocking.Par

  def parMonad = new Monad[Par] {
    def unit[A](a: => A): Par[A] = Par.lazyUnit(a)
    override def flatMap[A, B](ma: Par[A])(f: A => Par[B]): Par[B] =  Par.flatMap(ma)(f)
  }

  def listMonad = new Monad[List] {
    def unit[A](a: => A): List[A] = List[A](a)
    override def flatMap[A, B](ma: List[A])(f: A => List[B]): List[B] = ma flatMap f
    
  }

  val optionMonad = new Monad[chapter4.Option] {
    def unit[A](a: => A) = chapter4.Some(a)
    override def flatMap[A,B](ma: chapter4.Option[A])(f: A => chapter4.Option[B]) = ma flatMap f
  }

  def eitherMonad[S] = new  Monad[({type f[x] = chapter4.Either[S, x]}) #f]  {
    override def flatMap[A,B](ma: chapter4.Either[S,A])(f: A => chapter4.Either[S,B]): chapter4.Either[S,B] = ma flatMap f
     
    def unit[A](a: => A): chapter4.Either[S,A] = chapter4.Right(a)
  }

  val F = stateMonad[Int]
  def stateMonad[S] = new  Monad[({type s[x] = State[S, x]}) #s]  {
    override def flatMap[A,B](sa: State[S,A])(f: A => State[S,B]): State[S,B] = sa flatMap f

    def unit[A](a: => A): State[S,A] = State(s => (a, s))
  }

  def getState[S]: State[S,S] = State(s => (s,s))
  def setState[S](s: S): State[S,Unit] = State(_ => ((),s))
  def zipWithIndex[A](as: List[A]): List[(Int, A)] =
    as.foldLeft(F.unit(List[(Int, A)]()))((acc, a) => for {
      xs <- acc
      n <- getState
      _ <- setState(n + 1)
    } yield (n, a) :: xs).run(0)._1.reverse
}
