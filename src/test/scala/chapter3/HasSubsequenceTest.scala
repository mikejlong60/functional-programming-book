package chapter3

import org.scalacheck._
import Prop.{forAll, propBoolean}

object HasSubsequenceTest extends Properties("List has subsequence") {
  property("Test hasSubsequence with Ints") = {
    val sup = Cons(1, Cons(2, Cons(3, Cons(4, Nil))))

    List.hasSubsequence(sup, Cons(1, Nil)) == (true)

    List.hasSubsequence(sup, Cons(3, Nil)) == (true)

    List.hasSubsequence(sup, Cons(3, Cons(4, Nil))) == (true)

    List.hasSubsequence(sup, Cons(4, Cons(4, Nil))) == (false)

    List.hasSubsequence(sup, Cons(1, Cons(2, Cons(3, Cons(4, Nil))))) == (true)

    List.hasSubsequence(sup, Cons(1, Cons(2, Cons(3, Cons(5, Nil))))) == (false)

    List.hasSubsequence(sup, Cons(1, Cons(2, Cons(3, Cons(4, Cons(5, Nil)))))) == (false)

    List.hasSubsequence(sup, Nil) == (true)

    List.hasSubsequence(Nil, Cons(1, Nil)) == (false)

    List.hasSubsequence(Nil, Nil) == (true)
  }

  property("Test hasSubsequence never blows up") =
    forAll { (sup: Array[Int], sub: Array[String]) =>
      (List.hasSubsequence(List(sup: _*), List(sub: _*)))
      true
    }

}
