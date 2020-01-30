package chapter12

import org.scalatest.prop.PropertyChecks
import org.scalatest.{Matchers, PropSpec}

class ApplicativeTest extends PropSpec with PropertyChecks with Matchers {

  import ApplicativeInstances._

  property("Test successful validator mapN ") {
    def gt5(x: Int) =
      if (x > 5) Success(x)
      else Failure(s"x was $x which is not greater than 5")

    val actual = validation.map5(gt5(11), gt5(12), gt5(13), gt5(14), gt5(15))((a, b, c, d, e) => (a, b, c, d, e))
    actual should be (Success(11,12,13,14,15))
  }

  property("Test failure validator mapN ") {
    def lt5(x: Int) =
      if (x < 5) Success(x)
      else Failure(s"x was $x which is not less than 5")

    val actual = validation.map5(lt5(11), lt5(12), lt5(13), lt5(3), lt5(15))((a, b, c, d, e) => (a, b, c, d, e))
    actual should be (Failure("x was 15 which is not less than 5",Vector("x was 13 which is not less than 5", "x was 12 which is not less than 5", "x was 11 which is not less than 5")))
  }

  property("Test list mapN ") {
    forAll{ xs: List[Int] =>
      val sxs = xs.map(x => x.toString)
      val sxss = sxs.map(x => s"whatever $x")
      val actual = list.map3(xs, sxs, sxss)((a, b, c) => (a, b, c))
      val expected = ((xs zip sxs) zip sxss).map(xss => (xss._1._1, xss._1._2, xss._2))
      actual should be (expected)
    }
  }

  property("Verify associative law for list") {
    forAll{ (a: List[Int], b: List[Int], c: List[Int]) =>
      val fs = (list.unit(a), list.unit(b), list.unit(c))
      list.associativeLaw(fs._1)(fs._2)(fs._3) should be (true)
    }
  }

  property("Verify associative law for validator") {
    forAll{ (a: List[Int], b: List[Int], c: List[Int], fail: Boolean) =>
      val fs = (validation.unit(a), validation.unit(b), if (fail) Failure("dang") else validation.unit(c))
      validation.associativeLaw(fs._1)(fs._2)(fs._3) should be (true)
    }
  }

  property("Verify naturality law for list") {
    forAll{ (ln: List[Int], ln2: List[Int]) =>
      val ls = ln2.map(_.toString)
      val fln = list.unit(ln)
      val fls = list.unit(ls)
      val ts = (n: List[Int]) => n.map(_.toString)
      val tn = (sn: List[String]) => sn.map(_.toInt)

      val r = list.naturalityOfProductLaw(fln, fls)(ts)(tn)
      r should be (true)
    }
  }

  property("Verify naturality law for validator") {
    forAll{ (ln: List[Int], ln2: List[Int]) =>
      val ls = ln2.map(_.toString)
      val fln = validation.unit(ln)
      val fls = validation.unit(ls)
      val ts = (n: List[Int]) => n.map(_.toString)
      val tn = (n2: List[String]) => n2.map(_.toInt)
      val r = validation.naturalityOfProductLaw(fln, fls)(ts)(tn)
      r should be (true)
    }
  }

  property("Verify left and right identity law for list") {
    forAll{ (ln: List[Int]) =>
      val fln = list.unit(ln)
      val r = list.leftAndRightIdentityLaw(fln)
      r should be (true)
    }
  }

  property("Verify left and right identity law for validator") {
    forAll{ (ln: List[Int]) =>
      val fln = validation.unit(ln)
      val r = validation.leftAndRightIdentityLaw(fln)
      r should be (true)
    }
  }

  property("Understand stream mapN") {
    forAll{(a: Int, b: Int, c: String) =>
      val actual = stream.map3(stream.unit(a), stream.unit(b), stream.unit(c))((a, b, c) => (a, b, c)).take(300).toList
      actual.size should be (300)
    }
  }

  property("Understand stream sequence") {
    forAll{(a: Int, b: Int, c: Long) =>
      val s = List(stream.unit(a), stream.unit(b), stream.unit(c))
      val actual = stream.sequence(s).take(3).toList
      actual should be (List.fill(3)(List(a, b, c)))
    }
  }


}
