package chapter9

import org.scalatest.{ Matchers, PropSpec }
import org.scalatest.prop.PropertyChecks
import chapter6.{SimpleRNG, RNG, State}


class ParserTest extends PropSpec with PropertyChecks with Matchers {

  property("Run the char parser") {
    forAll{c: Char =>
   //   val actual = run(c)(c.toString)
      val expected = Right(c)
     // actual should be expected
    }
  }
}


