package chapter12

import org.scalatest.prop.PropertyChecks
import org.scalatest.{Matchers, PropSpec}
import chapter5.Stream

class MonadTest extends PropSpec with PropertyChecks with Matchers {

  import MonadInstances._

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
}
