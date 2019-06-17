package chapter8

import chapter6.RNG
import chapter5.Stream

object types {
  type SuccessCount = Int
  type FailedCase = String
  type TestCases = Int
}

sealed trait CausedBy
case object First extends CausedBy
case object Second extends CausedBy

import types._

sealed trait Result {
  def isFalsified: Boolean
  def fstFailure: Int
}

case class Passed(fstFailure: Int) extends Result  {
  def isFalsified = false
}

case class Falsified(fstFailure: Int, failure: FailedCase, successes: SuccessCount) extends Result  {
  def isFalsified = true
}

case class Prop(run:  (Int, TestCases, RNG) => Result)  {

  def &&(p: Prop): Prop = Prop {
     (fstFailure, n,  rng) => run(fstFailure, n, rng) match {
      case Passed(fst) => p.run(fst + 1, n, rng)
       case Falsified(fst, failure, successes) =>  Falsified(fst, failure, successes)
    }
  }

  def ||(p: Prop): Prop = Prop {
    (fstFailure, n, rng) => run(fstFailure, n, rng) match {
      case Passed(_) => Passed(fstFailure + 1)
      case Falsified(_, failure, successes) => p.run(fstFailure + 1, n, rng) 
    }
  }
}
 //   val r1 = Prop {()}
  // if (check(p1.run()) && p2.check)
  //    Prop {
  //      def check = true
  //    }
  //    else {
  //      new PropT {
  //        def check = false
  //      }
  //    }


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

object Prop {
  def forAll[A](as: Gen[A])(f: A => Boolean): Prop = Prop {
    (fstFailure, n, rng) => {
      val r = Stream.zip(randomStream(as)(rng), Stream.from(0)).take(n).map {
        case (a, i) => try {
          if (f(a)) Passed(fstFailure) else Falsified(fstFailure, a.toString, i)
        } catch { case e: Exception => Falsified(fstFailure, buildMsg(a, e), i) }
      }
      Stream.find(r)(c => c.isFalsified).getOrElse(Passed(0))
    }
  }

  def randomStream[A](g: Gen[A])(rng: RNG): Stream[A] = Stream.unfold(rng)(rng => Some(g.sample.run(rng)))

  def buildMsg[A](s: A, e: Exception): String =
    s"test case: $s\n" +
  s"generated an exception: ${e.getMessage}\n" +
  s"stack trace: \n ${e.getStackTrace.mkString("\n")}"


  def check(p: => Boolean): Prop = Prop { (fstFailure, _, _) =>
      if (p) Passed(fstFailure) else Falsified(fstFailure, "()", 0)
  }

}
