package chapter14

import org.scalatest.prop.PropertyChecks
import org.scalatest.{Matchers, PropSpec}

class RunnableSTTest extends PropSpec with PropertyChecks with Matchers {

  property("test running first program to produce something") {
    forAll {(x:  Int, y: Int) =>
      val swapper = new RunnableST[(Int, Int)] {
        def apply[S] = for {
          r1 <- STRef(x)
          r2 <- STRef(y)
          x <- r1.read
          y <- r2.read
          _ <- r1.write(y + 1)
          _ <- r2.write(x + 1)
          a <- r1.read
          b <- r2.read
        } yield (a, b)
      }

      val actual = ST.runST(swapper)
      actual should be ((y + 1, x + 1))
    }
  }
}
