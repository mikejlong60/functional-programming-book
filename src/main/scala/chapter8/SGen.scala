package chapter8

case class SGen[+A](forSize: Int => Gen[A]) {

  def apply(n: Int): Gen[A] = forSize(n)

  def map[B](f: A => B): SGen[B] = {
    val fff = forSize(_: Int).map(c => f(c))//  f
    SGen(fff)
  }

  def flatMap[B](f: A => SGen[B]): SGen[B] = {
    val forSize2: Int => Gen[B] = n => {
      forSize(n) flatMap {a => f(a).forSize(n) }
    }
    SGen(forSize2)
  }

  def nonEmptyListOf[A](g: Gen[A]): SGen[List[A]] = {
    val fs: Int => Gen[List[A]] = n => Gen.listOfN(n max 1, g)
    SGen(fs)
  }

  def listOf[A](g: Gen[A]): SGen[List[A]] = {
   val fs: Int => Gen[List[A]] = n =>  Gen.listOfN(n, g)
   SGen(fs)
  }
}
