package chapter7.nonblocking

import Nonblocking.Par._
import java.util.concurrent._
import org.scalacheck._
import Prop.{forAll, propBoolean}

object NonBlockingParTest extends Properties("Non-blocking Par tests") {

  val executor = Executors.newFixedThreadPool(5)

  property("run choice") =
    forAll {choice: Boolean =>
      val t = lazyUnit("it was true")
      val f = lazyUnit("it was false")
      val a = Nonblocking.Par.choice(unit(choice))(t, f)
      val actual = Nonblocking.Par.run(executor)(a).get
      if (choice) actual == "it was true"
      else actual == "it was false"
    }

  property("run choiceUsingChoiceN") =
    forAll {choice: Boolean =>
      val t = lazyUnit("it was true")
      val f = lazyUnit("it was false")
      val a = Nonblocking.Par.choiceUsingChoiceN(unit(choice))(t, f)
      val actual = Nonblocking.Par.run(executor)(a).get
      if (choice) actual == "it was true"
      else actual =="it was false"
    }

  property("run choiceUsingChooser") =
    forAll {choice: Boolean =>
      val t = lazyUnit("it was true")
      val f = lazyUnit("it was false")
      val a = Nonblocking.Par.choiceUsingChooser(unit(choice))(t, f)
      val actual = Nonblocking.Par.run(executor)(a).get
      if (choice) actual == "it was true"
      else actual == "it was false"
    }

  property("run choiceN") =
    forAll {choice: Boolean =>
      val t = lazyUnit("it was true")
      val f = lazyUnit("it was false")
      val c = if (choice) 0 else 1
      val a = Nonblocking.Par.choiceN(unit(c))(List(t, f))
      val actual = Nonblocking.Par.run(executor)(a).get
      if (choice) actual == "it was true"
      else actual == "it was false"
    }

  property("run choiceNUsingChooser") =
    forAll {choice: Boolean =>
      val t = lazyUnit("it was true")
      val f = lazyUnit("it was false")
      val c = if (choice) 0 else 1
      val a = Nonblocking.Par.choiceNUsingChooser(unit(c))(List(t, f))
      val actual = Nonblocking.Par.run(executor)(a).get
      if (choice) actual == "it was true"
      else actual == "it was false"
    }

  property("run flatMap.  It used to be called chooser.") =
    forAll { choice: Boolean =>
      val t = lazyUnit("it was true")
      val f = lazyUnit("it was false")
      val c = if (choice) 0 else 1
      val a = Nonblocking.Par.flatMap(unit(c))(List(t, f))
      val actual = Nonblocking.Par.run(executor)(a).get
      if (choice) actual == "it was true"
      else actual == "it was false"
    }

  property("run flatMap that uses join. ") =
    forAll {choice: Boolean =>
      val t = lazyUnit("it was true")
      val f = lazyUnit("it was false")
      val c = if (choice) 0 else 1
      val a = Nonblocking.Par.flatMapThatUsesJoin(unit(c))(List(t, f))
      val actual = Nonblocking.Par.run(executor)(a).get
      if (choice) actual == "it was true"
      else actual == "it was false"
    }

  property("run join. ") =
    forAll {choice: Boolean =>
      val c = if (choice) 1 else 0
      val cc = unit(c)
      val ccc = unit(map(cc)(c => c))
      val a = join(ccc)
      val actual = Nonblocking.Par.run(executor)(a).get
      if (choice) actual == 1
      else actual == 0
    }

  property("run join that uses flatmap. ") =
    forAll {choice: Boolean =>
      val c = if (choice) 1 else 0
      val cc = unit(c)
      val ccc = unit(map(cc)(c => c))
      val a = joinThatUsesFlatMap(ccc)
      val actual = Nonblocking.Par.run(executor)(a).get
      if (choice) actual == 1
      else actual == 0
    }

  property("prove that parMap does not deadlock, even with a thread pool of only 1 thread.") = {
    val ex = Executors.newFixedThreadPool(1)
    val xs = (1 to 1000).toList
    val a = Nonblocking.Par.parMap(xs)(math.sqrt(_))
    val actual = Nonblocking.Par.run(ex)(a).get
    val expected = xs.map(math.sqrt(_))
    actual ==expected
  }

  def lawOfFork[A](a: Nonblocking.Par[A])(es: ExecutorService): Boolean = {
    val aa = Nonblocking.Par.run(es)(fork(a))
    val bb = Nonblocking.Par.run(es)(a)
    aa == bb
  }

  property("law of fork") =
    forAll{(x: Int, xs: List[Int]) =>
      val g  = (i: Int) =>  i + x
      lawOfFork(unit(g))(executor) == true
      lawOfFork(unit(x))(executor) == true
      lawOfFork(unit(xs))(executor) == true
    }

  property("law of map") =
    //Law of map says:
    // map(y)(id) == y ////where y is any Par and id is the identity function that just returns its parameter unchanged.
    forAll { x: Int =>
      val id  = (i: Int) =>  i
      val y = unit(x)
      val actual = Nonblocking.Par.run(executor)( map(y)(id)).get
      val expected = Nonblocking.Par.run(executor)(y).get
      actual == expected
    }

  //Concluding ponderings in response to the following three questions at the end of Chapter 7:
  // 1. Question -  Can you implement a function with the same signature as map2 that uses flatMap and unit?
  // Answer -  Yes. See last test below.  But it is different than the other parallel version of map2 in that it waits for
  //the first computation to complete before doing the other. The implementation that uses flatMap and unit also uses two Future
  //which is the implementation detail that forces the second to always wait for the first to complete.
  //The other implementation fired them both off in parallel and the one that finished first was pattern matched
  //and used by the second when it finished.
  //2. Question - Can you think of laws governing how join relates to the other primitives of the algebra? 
  //Answer - Join is the same as flatMapping over the identity function.
  //3. Question - What parallel computations cannot be expressed using this algebra?  Think about this for tomorrow.
  property("run map2 that uses flatmap and unit. ") =
    forAll {(x: Int, y: Int) =>
      val sum = (x:Int, y: Int) => x + y
      val a = map2ThatUsesJoinAndFlatmap(unit(x), unit(y))(sum)
      val actual = Nonblocking.Par.run(executor)(a).get
      actual ==  (x + y)
    }

  property("run map2 that uses flatmap and unit with 2 functions. ") =
    forAll {x: Int =>
      val cube = (x: Int) => x * x * x
      val square = (x: Int) => x  * x
      val cubeU = unit(cube)
      val squareU = unit((x: Int) => x * x)
      val sum = (z: Int => Int, z2: Int => Int) => z(x) + z2(x)
      val a = map2ThatUsesJoinAndFlatmap(cubeU, squareU)(sum)
      val actual = Nonblocking.Par.run(executor)(a).get
      actual == (cube(x) + square(x))
    }

  property("run regular map2 with 2 functions. ") =
    forAll {x: Int =>
      val cube = (x: Int) => x * x * x
      val square = (x: Int) => x  * x
      val cubeU = unit(cube)
      val squareU = unit((x: Int) => x * x)
      val sum = (z: Int => Int, z2: Int => Int) => z(x) + z2(x)
      val a = map2(cubeU, squareU)(sum)
      val actual = Nonblocking.Par.run(executor)(a).get
      actual == (cube(x) + square(x))
    }

  property("run regular map2. ") =
    forAll {(x: Int, y: Int) =>
      val sum = (x:Int, y: Int) => x + y
      val a = map2(unit(x), unit(y))(sum)
      val actual = Nonblocking.Par.run(executor)(a).get
      actual == (x + y)
    }
}

