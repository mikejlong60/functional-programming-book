package chapter4

import org.scalatest.prop.PropertyChecks
import org.scalatest.{Matchers, PropSpec}
import scala.{Option => _, None => _, Some => _,  Either => _, _}
import scala.math

class VarianceTest extends PropSpec with PropertyChecks with Matchers {

  def variance(xs: Seq[Double]): Option[Double] = 
    if (xs.isEmpty) None
    else Some(xs)
      .flatMap(xs => {
        val m = xs.sum /xs.length
        val r = xs.map(x => math.pow(x -m,  2))
        Some(r.sum / r.length)
      })
  

  property("Test variance function") {
    forAll { xs: Seq[Double] =>
      val actual = variance(xs)
      if (xs.isEmpty) actual should be (None)
      else actual should be {
        val m = xs.sum / xs.length
        val r = xs.map(x => math.pow(x  -m, 2))
        Some(r.sum / r.length)
      }
    }
  }
}
