package chapter12

import org.scalacheck._
import Prop.{forAll, propBoolean}

object  ApplicativeTest extends Properties("Applicative test") {

  import ApplicativeInstances._

  property("Test successful validator mapN ") = {
    def gt5(x: Int) =
      if (x > 5) Success(x)
      else Failure(s"x was $x which is not greater than 5")

    val actual = validation.map5(gt5(11), gt5(12), gt5(13), gt5(14), gt5(15))((a, b, c, d, e) => (a, b, c, d, e))
    actual == Success(11,12,13,14,15)
  }

  property("Test failure validator mapN ") = {
    def lt5(x: Int) =
      if (x < 5) Success(x)
      else Failure(s"x was $x which is not less than 5")

    val actual = validation.map5(lt5(11), lt5(12), lt5(13), lt5(3), lt5(15))((a, b, c, d, e) => (a, b, c, d, e))
    actual == (Failure("x was 15 which is not less than 5",Vector("x was 13 which is not less than 5", "x was 12 which is not less than 5", "x was 11 which is not less than 5")))
  }

  property("Test list mapN ") =
    forAll{ xs: List[Int] =>
      val sxs = xs.map(x => x.toString)
      val sxss = sxs.map(x => s"whatever $x")
      val actual = list.map3(xs, sxs, sxss)((a, b, c) => (a, b, c))
      val expected = ((xs zip sxs) zip sxss).map(xss => (xss._1._1, xss._1._2, xss._2))
      actual == (expected)
    }

  property("Verify associative law for list") =
    forAll{ (a: List[Int], b: List[Int], c: List[Int]) =>
      val fs = (list.unit(a), list.unit(b), list.unit(c))
      list.associativeLaw(fs._1)(fs._2)(fs._3)
    }


  property("Verify associative law for validator") =
    forAll{ (a: List[Int], b: List[Int], c: List[Int], fail: Boolean) =>
      val fs = (validation.unit(a), validation.unit(b), if (fail) Failure("dang") else validation.unit(c))
      validation.associativeLaw(fs._1)(fs._2)(fs._3)
    }

  property("Verify naturality law for list") =
    forAll{ (ln: List[Int], ln2: List[Int]) =>
      val ls = ln2.map(_.toString)
      val fln = list.unit(ln)
      val fls = list.unit(ls)
      val ts = (n: List[Int]) => n.map(_.toString)
      val tn = (sn: List[String]) => sn.map(_.toInt)

      list.naturalityOfProductLaw(fln, fls)(ts)(tn)
    }

  property("Verify naturality law for validator") =
    forAll{ (ln: List[Int], ln2: List[Int]) =>
      val ls = ln2.map(_.toString)
      val fln = validation.unit(ln)
      val fls = validation.unit(ls)
      val ts = (n: List[Int]) => n.map(_.toString)
      val tn = (n2: List[String]) => n2.map(_.toInt)
      validation.naturalityOfProductLaw(fln, fls)(ts)(tn)
    }

  property("Verify left and right identity law for list") =
    forAll{ (ln: List[Int]) =>
      val fln = list.unit(ln)
      list.leftAndRightIdentityLaw(fln)
    }

  property("Verify left and right identity law for validator") =
    forAll{ (ln: List[Int]) =>
      val fln = validation.unit(ln)
      validation.leftAndRightIdentityLaw(fln)
    }

  property("Understand stream mapN") =
    forAll{(a: Int, b: Int, c: String) =>
      val actual = stream.map3(stream.unit(a), stream.unit(b), stream.unit(c))((a, b, c) => (a, b, c)).take(300).toList
      actual.size == 300
    }

  property("Understand stream sequence") =
    forAll{(a: Int, b: Int, c: Long) =>
      val s = List(stream.unit(a), stream.unit(b), stream.unit(c))
      val actual = stream.sequence(s).take(3).toList
      actual ==List.fill(3)(List(a, b, c))
    }

  property("goof around with product of two applicatives") =
    forAll{(a: Int)=>
      val o: (Option[Int], Option[Int]) = (option.unit(a), option.unit(a))
      val of = (option.unit((a: Int) => a + 1), option.unit((b: Int) => b  - 1))
      val g = option.productG(option)
      val actual = g.apply(of)(o)
      actual == (Some(a+1), (Some(a-1)))
    }

  property("goof around with other composition of two applicatives") =
    forAll{(a: Int)=>
      val g = option.composeG(option)
      val actual: Option[Option[Option[Int]]] = g.unit(option.unit(a))
      true
    }

    property("Verify associative law for composeG") =
      forAll{ (a: Int, b: Int, c: Int) =>
        val g = option.composeG(option)
        val a1: Option[Option[Option[Int]]] = g.unit(option.unit(a))
        val a2: Option[Option[Option[Int]]] = g.unit(option.unit(b))
        val a3: Option[Option[Option[Int]]] = g.unit(option.unit(c))

        g.associativeLaw(a1)(a2)(a3)
      }

  property("Verify left and right identity law for composeG") =
    forAll{ (ln: Int) =>
      val g = option.composeG(option)
      val i: Option[Option[Int]] = g.unit(ln)
      g.leftAndRightIdentityLaw(i)
    }

  property("Verify naturality law for composeG") =
    forAll{ (ln: Int, ln2: Int) =>
      val g = option.composeG(option)
      val fln = option.unit(ln)
      val fls = option.unit(ln2.toString)
      val ts = (n: Int) => n.toString
      val tn = (sn: String) => sn.toInt

      option.naturalityOfProductLaw(fln, fls)(ts)(tn)
    }

  property("Test sequenceMap for option applicative") =
    forAll{mk: Map[Int, Option[String]] =>
      val actual: Option[Map[Int, String]] = option.sequenceMap(mk)
      (mk.exists(kv => kv._2.isEmpty), mk.isEmpty, actual) match {
        case (true, _, _) => actual == None
        case (false, true, _) => actual == Some(Map())
        case (false, false, Some(a))  => a.size == mk.size
        case _ => false
      }
  }
}
