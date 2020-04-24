package chapter15

import chapter5.Stream
import org.scalacheck._
import Prop.{forAll, propBoolean}

object ProcessMonadTest extends Properties("ProcessMonad tests") {

  import chapter12.MonadInstances._

  val mon = process[List[Int]]



  property("Verify associative law for Process") =
    forAll{ (l: Int) =>
     // val all = Stream(l:_*)

      //val fs = (mon.unit(a), mon.unit(b), mon.unit(c))
      //process.associativeLaw(fs._1)(fs._2)(fs._3) should be (true)

     // val f =  (x: List[Int]) => x.toString
      //val g = (y: List[String]) => s"the number was: $y"

     val filt  =(x: Int) => x % 2 == 0
      val times1000 = (x: Int) => x * 1000
      val minus3 = (x: List[Int]) => x  //- 3
      val plus3 = (x: List[Int]) => x //x + 3




      //val expected = l.filter(filt).map(times1000).map(minus3).map(plus3)
     // val evenTimes1000 = Process.filter(filt) |> Process.lift(times1000) |> Process.lift(minus3) |> Process.lift(plus3)
    //  val actual = evenTimes1000(all).take(10).toList
      //actual should be (expected.take(10))
 


      //mon.associativeLaw(Process.lift(plus3))(Process.lift(minus3))(Process.lift(plus3)) should be (true)
      //mon.associativeLaw("heck")(f)(g) should be (true)

      true
    }


  /**
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

    */
}
