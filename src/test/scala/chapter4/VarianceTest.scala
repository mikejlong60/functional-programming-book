package chapter4

import scala.{Option => _, None => _, Some => _,  Either => _, _}
import org.scalacheck._
import Prop.forAll

object VarianceTest extends Properties("Variance tests") {

  def variance(xs: Seq[Double]): Option[Double] =
    if (xs.isEmpty) None
    else Some(xs)
      .flatMap(xs => {
        val m = xs.sum /xs.length
        val r = xs.map(x => math.pow(x -m,  2))
        Some(r.sum / r.length)
      })
  

  property("Test variance function") =
    forAll { xs: Seq[Double] =>
      val actual = variance(xs)
      if (xs.isEmpty) actual == None
      else actual == {
        val m = xs.sum / xs.length
        val r = xs.map(x => math.pow(x - m, 2))
        Some(r.sum / r.length)
      }
    }
}
