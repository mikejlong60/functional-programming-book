package chapter8

import chapter6.RNG
import chapter5.Stream

object types {
  type SuccessCount = Int
  type FailedCase = String
  type TestCases = Int
}

import types._

sealed trait Result {
  def isFalsified: Boolean
}

case object Passed extends Result  {
  def isFalsified = false
}

case class Falsified(failure: FailedCase, successes: SuccessCount) extends Result  {
  def isFalsified = true
}

case class Prop(run:  (TestCases, RNG) => Result) {
 // def check: Boolean
//  def &&(p: Prop): Prop =
//    if (p.check && this.check)
//      new PropT {
//        def check = true
//      }
//      else {
//        new PropT {
//          def check = false
//        }
//      }

  def forAll[A](as: Gen[A])(f: A => Boolean): Prop = Prop {
    (n, rng) => {
      val r = Stream.zip(randomStream(as)(rng), Stream.from(0)).take(n).map {
        case (a, i) => try {
          if (f(a)) Passed else Falsified(a.toString, i)
        } catch { case e: Exception => Falsified(buildMsg(a, e), i) }
      }
      Stream.find(r)(_.isFalsified).getOrElse(Passed)
    }
  }

  def randomStream[A](g: Gen[A])(rng: RNG): Stream[A] = Stream.unfold(rng)(rng => Some(g.sample.run(rng)))

  def buildMsg[A](s: A, e: Exception): String =
    s"test case: $s\n" +
  s"generated an exception: ${e.getMessage}\n" +
  s"stack trace: \n ${e.getStackTrace.mkString("\n")}"

}
