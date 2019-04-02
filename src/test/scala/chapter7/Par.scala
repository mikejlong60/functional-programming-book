package chapter7

trait Par[A] {
  def unit(a:  => A): Par[A]
  def get(a: Par[A]): A
  def map2[A, B, C] (a: Par[A], b: Par[B]) (f: (A, B) => C):Par[C] = ???
  
}

object Par {

  def sumInParallelNot(ints: List[Int]): Int = {
    if (ints.size <= 1) ints.headOption getOrElse 0
    else {
      val (l, r) = ints.splitAt(ints.length/2)
      sumInParallelNot(l) + sumInParallelNot(r)
    }
  }

  def sumInParallel[A](ints: List[Int], myPar: Par[Int]): Int = {
    if (ints.size <= 1) {
      val p = myPar.unit(ints.headOption getOrElse 0)
      12//myPar.get(p)// This is dumb
    } else {
      val (l, r) = ints.splitAt(ints.length/2)
      sumInParallel(l, myPar) + sumInParallel(r, myPar)
    }
  }


}

case class MyPar[String](x: String) extends Par[String] {
  def unit(a: => String): MyPar[String] = MyPar(a)
  def get(a: Par[String]):String =  ???
}
