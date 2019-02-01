package chapter5


sealed trait Stream[+A] 
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

  def apply[A](as: A*): Stream[A] = if (as.isEmpty) empty else cons(as.head, apply(as.tail: _*))


  def drop[A](n: Int)(s: Stream[A]): Stream[A] = s match {
    case Empty => Empty
    case Cons(_, t) if n > 0 => drop(n - 1)(t())
    case Cons(_, t) => s
  }

    def toList[A](s: Stream[A]): List[A] = s match {
      case Cons(h, t) => h() +: toList(t())
      case _ => Nil
  }

}


