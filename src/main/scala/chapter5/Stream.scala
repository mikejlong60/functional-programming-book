package chapter5


sealed trait Stream[+A] {
  @annotation.tailrec
  final def drop(n: Int): Stream[A] = this match {
    case Cons(_, t) if n > 0 => t(). drop(n - 1)
    case  _ => this
  }

  final def toList:List[A] = this match {
     case Cons(h, t) => h() +: t().toList
      case _ => Nil
  }

  /*
    Create a new Stream[A] from taking the n first elements from this. We can achieve that by recursively
    calling take on the invoked tail of a cons cell. We make sure that the tail is not invoked unless
    we need to, by handling the special case where n == 1 separately. If n == 0, we can avoid looking
    at the stream at all.
   */
  final def take(n: Int): Stream[A] = this match {
    case Cons(h, t) if n > 1 => Stream.cons( h(), t().take(n-1))
    case Cons(h, _) if n == 1 => Stream.cons(h(), Stream.empty)
    case _ => Stream.empty
  }

  //This guy stops evaluation immediately after condition is not met and returns the resulting stream.  Note that
  //its lazy.  The tail never gets evaluated after that so you can do it for an infinite list.
  final def takeWhile2(p : A => Boolean): Stream[A] = this match {
    case Cons(h, t) if p(h()) => Stream.cons(h(), t().takeWhile2(p))
    case _ => Stream.empty
  }

  final  def foldRight[B](z: => B)(f: (A, => B) => B): B = this match {
    case Cons(h, t) => f(h(), t().foldRight(z)(f))
    case _ => z
  }

  final def map[B](f: A => B): Stream[B] = foldRight(Stream.empty[B])((a, b) => Stream.cons(f(a), b))

  final def flatMap[B >: A](f: A => Stream[B]): Stream[B] = foldRight(Stream.empty[B])((a, b) => f(a) append(b))

  final def append[B >: A](l2: Stream[B]): Stream[B] = foldRight(l2)( (a, b) => Stream.cons(a, b))

  final def tails2: Stream[Stream[A]] = {
    val r = foldRight(Stream.empty[Stream[A]])((a, b) => (a, b) match {
      case (aa, Empty) => Stream.cons(Stream.cons(aa, Empty), b)
      case (aa, s @ Cons(h, t)) => Stream.cons(Stream.cons(aa, h()), b)
      case _ => b append Stream.empty
    })
    r append Stream(Empty)
  }

  // I cheated on this and looked up the answer after trying to do it for 8 hours.  I forgot about being able to use append. I had
  // it working except for the empty stream at the end.  I also forgot about drop to remove the head and was using pattern matching
  // to extract the head and tail from xs.
  def tails: Stream[Stream[A]] =
    Stream.unfold(this) {
      case Empty => None
      case s => Some((s, s drop 1))
    } append Stream(Empty)

  //Version based upon foldRight
  final def takeWhile(p : A => Boolean): Stream[A] = foldRight(Stream.empty[A])((a, b) => 
    if (p(a))  Stream.cons(a, b)
    else Stream.empty
  )

  def exists(p: A => Boolean): Boolean = foldRight(false)((a, b) => p(a) || b)

  def filter(p: A => Boolean): Stream[A] = foldRight(Stream.empty[A])((a, b) => 
    if (p(a)) Stream.cons(a, b)
    else b
  )

  //This was busted because I needed to assert that p(a) was true AND b was true, not p(a) was true OR b was true.  So
  //it busted sometimes and worked sometimes.
  def forAll3(p: A => Boolean): Boolean = !exists(p)

  def forAll(p: A => Boolean): Boolean = foldRight(true)((a, b) => p(a) && b)
}

case object Empty extends Stream[Nothing]
case class Cons[+A](h: () => A, tl: () => Stream[A]) extends Stream[A]

object Stream {
  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    val hf = () => head
    val tf = () => tail
    val r = Cons(hf, tf)
    r
  }

  def empty[A]: Stream[A] = Empty


  //Latest additions on 6/08/2019.  There were no exercises for this so I stole them from answer key for use with later exercises.
  def zipWith[A, B,C](s1 :Stream[A], s2: Stream[B])(f: (A,B) => C): Stream[C] =
    unfold((s1, s2)) {
      case (Cons(h1,t1), Cons(h2,t2)) =>
        Some((f(h1(), h2()), (t1(), t2())))
      case _ => None
    }

  // special case of `zipWith`
  def zip[A, B](s1: Stream[A], s2: Stream[B]): Stream[(A,B)] =
    zipWith(s1, s2)((_,_))

  def zipAll[A, B](s1: Stream[A], s2: Stream[B]): Stream[(Option[A],Option[B])] =
    zipWithAll(s1, s2)((_,_))

  def zipWithAll[A, B, C](s1: Stream[A], s2: Stream[B])(f: (Option[A], Option[B]) => C): Stream[C] =
    Stream.unfold((s1, s2)) {
      case (Empty, Empty) => None
      case (Cons(h, t), Empty) => Some(f(Some(h()), Option.empty[B]) -> (t(), empty[B]))
      case (Empty, Cons(h, t)) => Some(f(Option.empty[A], Some(h())) -> (empty[A] -> t()))
      case (Cons(h1, t1), Cons(h2, t2)) => Some(f(Some(h1()), Some(h2())) -> (t1() -> t2()))
    }

  @annotation.tailrec
  def find[A](s: Stream[A])(f: A => Boolean): Option[A] = s match {
    case Empty => None
    case Cons(h, t) => if (f(h())) Some(h()) else find(t())(f)
  }

  ///End stuff stolen for tests later in book 0n 6/8/2019

  def apply[A](as: A*): Stream[A] = if (as.isEmpty) empty else cons(as.head, apply(as.tail: _*))

  def constant[A](a: A): Stream[A] = cons(a, constant(a))

  def from(n: Int): Stream[Int] = cons(n, from(n+1))

  //This also works but Paul's is better.  But this will morph into unfold.
  def fibme(s: Stream[Int]): Stream[Int] = s match {
    case Empty => cons(0, fibme(cons(0, s)))
    case Cons(h, t) => t() match {
       case Empty => fibme(cons(1, s))
       case Cons(hh, tt) => cons(h(), fibme(cons(h() + hh(), s)))
    }
  }

  val fib = {
    def go(f0: Long, f1: Long): Stream[Long] = cons(f0, go(f1, f0+f1))
    go(0, 1)
  }

  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = f(z).map(tpl => cons(tpl._1, unfold(tpl._2)(f))).getOrElse(empty)

  // I cheated on this and looked up the answer after trying to do it for 8 hours.  I forgot about being able to use append. I had
  // it working except for the empty stream at the end.  I also forgot about drop to remove the head and was using pattern matching
  // to extract the head and tail from xs.
  def tails[A](xs: Stream[A]): Stream[Stream[A]] =
    unfold(xs) {
      case Empty => None
      case s => Some((s, s drop 1))
    } append Stream(empty)


  //Uses foldright
  final def tails2[A](xs: Stream[A]): Stream[Stream[A]] = {
    xs.foldRight(Stream.empty[Stream[A]])((a, b) => (a, b) match {
      case (aa, Empty) => Stream.cons(Stream.cons(aa, Empty), b)
      case (aa, Cons(h, t)) => Stream.cons(Stream.cons(aa, h()), b)
      case _ => b append Stream.empty
    }) append Stream(empty)
  }

  def scanRight[A, S](xs: Stream[A])(z: S)(op: (A, => S) => S): Stream[S] = xs.foldRight((z, Stream(z)))((a, p0) => {
    lazy val p1 =p0
    val b2 = op(a, p1._1)
    (b2, Stream.cons(b2, p1._2))
   })._2
}
