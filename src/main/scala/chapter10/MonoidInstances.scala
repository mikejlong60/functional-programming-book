package chapter10

object MonoidInstances {

  def mapMergeMonoid[K,V](V: Monoid[V]): Monoid[Map[K,V]] = new Monoid[Map[K,V]]  {
    def zero = Map[K,V] ()
    def op(a: Map[K,V], b: Map[K,V]) = (a.keySet ++ b.keySet).foldLeft(zero) ((acc, k) => acc.updated(k, V.op(a.getOrElse(k, V.zero),b.getOrElse(k, V.zero))))
  }


  def functionMonoid[A, B](m: Monoid[B]): Monoid[A => B] = new Monoid[A => B] {
    def op(f1: A => B, f2: A => B): A => B = (a: A) => m.op(f1(a), f2(a))
    def zero: A => B = (a: A) => m.zero
  }
    
  def productMonoid[A, B](a: Monoid[A], b: Monoid[B]) = new Monoid[(A, B)]  {

    def op(aa:  (A, B), bb: (A, B)): (A, B) = {
      val as = a.op(aa._1, bb._1)
      val bs = b.op(aa._2, bb._2)
      (as, bs)
    }
    def zero:(A, B) = (a.zero, b.zero)
  }

  val stringMonoid: Monoid[String] = new Monoid[String] {
    def op(a1: String, a2: String): String = a1 + a2
    def zero: String = ""
  }

  def listMonoid[A]: Monoid[List[A]] = new Monoid[List[A]] {
    def op(a1: List[A], a2: List[A]): List[A] = a1 ++ a2
    def zero: List[A] = Nil
  }

  val intAddition: Monoid[Int]  = new Monoid[Int] {
    def op(a1: Int, a2: Int): Int = a1 + a2
    def zero: Int = 0
  }

  val mySlightlyWrongintOrdered: Monoid[(Boolean, Int)]  = new Monoid[(Boolean, Int)] {
    def op(a1: (Boolean, Int), a2: (Boolean, Int)): (Boolean, Int) = {
    if (a1._1 && (a2._2 >= a1._2)) a2
    else if (a2 == zero) a2
    else (false, a1._2)
    }
    def zero: (Boolean, Int) = (true, Int.MinValue)
  }


    val intOrdered = new Monoid[Option[(Int, Int, Boolean)]] {
      def op(o1: Option[(Int, Int, Boolean)], o2: Option[(Int, Int, Boolean)]) =
        (o1, o2) match {
          // The ranges should not overlap if the sequence is ordered.
          case (Some((x1, y1, p)), Some((x2, y2, q))) =>
            Some((x1 min x2, y1 max y2, p && q && y1 <= x2))
          case (x, None) => x
          case (None, x) => x
        }
      val zero = None
    }
  
  val intMultiplication: Monoid[Int] = new Monoid[Int] {
    def op(a1: Int, a2: Int): Int = a1 * a2
    def zero: Int = 1
  }

  val booleanOr: Monoid[Boolean] = new Monoid[Boolean]  {
    def op(a1: Boolean, a2: Boolean): Boolean = if (a1) true else a2
    def zero: Boolean = false
  }

  val booleanAnd: Monoid[Boolean] = new Monoid[Boolean]  {
    def op(a1: Boolean, a2: Boolean): Boolean = if (a1) a2 else false
    def zero: Boolean = true
  }

  def endoMonoid[A]: Monoid[A => A] = new Monoid[A => A] {
    def op(f1: A => A, f2: A => A): A=> A = (a: A) => f1(f2(a))  //This is same as f2 andThen f1 or f1 compose f2
    def zero: A => A = (x: A) => x
  }

  def optionMonoid[A]: Monoid[Option[A]] = new Monoid[Option[A]] {
    def op(a1: Option[A], a2: Option[A]): Option[A] = a1 orElse a2
    def zero: Option[A] = None
  }

  def wcMonoid: Monoid[WC] = new Monoid[WC] {
    val zero: WC = Stub("")
    def op(a1: WC, a2: WC): WC = (a1, a2) match {
      case (Stub(l), Stub(r)) => Stub(l + r)
      case (Stub(c), Part(l, w, r)) => Part(c + l, w, r)
      case (Part(l,w,r), Stub(c)) => Part(l, w, r + c)
      case (Part(l1, w1, r1), Part(l2, w2, r2)) => Part(l1,  w1 + (if ((r1 + l2).isEmpty) 0 else 1) + w2, r2 )
    }
  }
}

sealed trait WC
case class Stub(chars: String) extends WC
case class Part(lStub: String, words: Int, rStub: String) extends WC
