package chapter3

import org.scalatest.prop.PropertyChecks
import org.scalatest.{Matchers, PropSpec}

class HasSubsequenceTest extends PropSpec with PropertyChecks with Matchers {

  property("Test hasSubsequence with Ints") {
    val sup = Cons(1, Cons(2, Cons(3, Cons(4, Nil))))

    List.hasSubsequence(sup, Cons(1, Nil)) should be (true)

    List.hasSubsequence(sup, Cons(3, Nil)) should be (true)

    List.hasSubsequence(sup, Cons(3, Cons(4, Nil))) should be (true)

    List.hasSubsequence(sup, Cons(4, Cons(4, Nil))) should be (false)

    List.hasSubsequence(sup, Cons(1, Cons(2, Cons(3, Cons(4, Nil))))) should be (true)

    List.hasSubsequence(sup, Cons(1, Cons(2, Cons(3, Cons(5, Nil))))) should be (false)

    List.hasSubsequence(sup, Cons(1, Cons(2, Cons(3, Cons(4, Cons(5, Nil)))))) should be (false)

    List.hasSubsequence(sup, Nil) should be (true)

    List.hasSubsequence(Nil, Cons(1, Nil)) should be (false)

    List.hasSubsequence(Nil, Nil) should be (true)
  }

  property("Test hasSubsequence never blows up") {
    forAll { (sup: Array[Int], sub: Array[String]) =>
      noException should be thrownBy (List.hasSubsequence(List(sup: _*), List(sub: _*)))
    }
  }

}
