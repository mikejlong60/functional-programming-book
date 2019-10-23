package chapter10

object MonoidInstances {
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
      println(s"$a2 : $a1")//a2 is fresh parm, a1 is accum
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
    def op(a1: WC, a2: WC): WC = (a1, a2) match {
      case (Part(_, _, _), Part(Stub(""), 0 , Stub(""))) => a1
      case (Part(Stub(""), n1, Stub("")), Part(Stub(""), n2, Stub(""))) => Part(Stub(""), n1 + n2, Stub(""))
      case (Part(ll, n1, Stub(lr)), Part(Stub(rl), n2, rr)) if lr.size > 0 || rl.size > 0 => Part(ll, n1 + n2 + 1, rr)
      case _ => a1
    }
    def zero: WC = Part(Stub(""), 0, Stub(""))
  }

}

sealed trait WC
case class Stub(chars: String) extends WC
case class Part(lStub: Stub, words: Int, rStub: Stub) extends WC
