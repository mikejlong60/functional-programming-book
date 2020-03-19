package chapter15

import org.scalatest.{Matchers, PropSpec}
import org.scalatest.prop.PropertyChecks
import chapter5.Stream

class ProcessTest extends PropSpec with PropertyChecks with Matchers {

  property("liftOne") {
    forAll{ l: List[String] =>
      whenever (l.size > 0) {
        val p = Process.liftOne((s: String) => s +"done")
        val actual = p(Stream(l:_*)).toList
        actual should be (List(l(0)+"done"))
      }
    }
  }

  property("lift a fixed-size list") {
    forAll{ l: List[String] =>
    //  val l = List("a","b")
      val p = Process.lift((s: String) => s +"done")
      val actual = p(Stream(l:_*)).toList
      actual should be (l.map(s => s +"done"))
    }
  }

  property("lift an infinite list") {
    val aas = Process.lift((_:Unit) => "a")(Stream.continually(()))
    val actual = aas.take(1200).toList
    actual should be (List.fill(1200)("a"))
    println(actual)
  }
}
