package chapter15

import java.io.{BufferedWriter, FileWriter}
import java.util
import java.util.{Set, TreeMap, TreeSet}

import chapter13.IO
import org.scalatest.{Matchers, PropSpec}
import org.scalatest.prop.PropertyChecks
import chapter5.Stream

class ProcessTest extends PropSpec with PropertyChecks with Matchers {

  property("make a Process that waits for one element and then stops") {
    forAll{ l: List[String] =>
      whenever (l.size > 0) {
        val p = Process.liftOne((s: String) => s +"done")
        val actual = p(Stream(l:_*)).toList
        actual should be (List(l(0)+"done"))
      }
    }
  }

  property("make a Process that appends `done` to all elements of a stream") {
    forAll{ l: List[String] =>
      val p = Process.lift((s: String) => s +"done")
      val actual = p(Stream(l:_*)).toList
      actual should be (l.map(s => s +"done"))
    }
  }

  property("make a Process that repeats some value forever") {
    val aas = Process.lift((_:Unit) => "a")(Stream.continually(()))
    val actual = aas.take(1200).toList
    actual should be (List.fill(1200)("a"))
  }

  property("make a Process that filters a stream using some predicate") {
    forAll{l: List[Int] =>
      val even = Process.filter((x: Int) => x % 2 == 0)
      val all = Stream(l:_*)

      val actual = even(all)
      actual.toList should be (l.filter(_ % 2 == 0))
    }
  }

  property("make a Process that sums a Stream") {
   forAll{l: List[Double] =>
     val all = Stream(l:_*)
     val actual = Process.sum(all).toList
     val expected = l.foldLeft(List.empty[Double])((acc, x) => acc match {
       case y :: xs => (x + y) +: acc
       case _ => x +: acc
     }).reverse
     actual.toList should be (expected)
    }
  }

  property("make a Process that takes a given number of elements from a Stream") {
    forAll{l: List[Double] =>
      whenever (l.size > 10) {
        val take = l.size - 5
        val all: Stream[Double] = Stream(l:_*)
        val p: Process[Double, Double] = Process.take(take)
        val actual = p(all)
        actual.toList should be (l.take(take))
      }
    }
  }

  property("make a Process that drops the first n elements from a Stream") {
    forAll{l: List[Double] =>
      whenever (l.size > 10) {
        val drop = l.size - 5
        val all: Stream[Double] = Stream(l:_*)
        val p: Process[Double, Double] = Process.drop(drop)
        val actual = p(all)
        actual.toList should be (l.drop(drop))
      }
    }
  }

  property("make a Process that takes elements from a Stream as long as the given predicate remains true") {
    forAll {l: List[Double] => 
      val takeP = (x: Double) => x > 12.0
      val all: Stream[Double] = Stream(l:_*)
      val p: Process[Double, Double] = Process.takeWhile(takeP)
      val actual = p(all)
      actual.toList should be (l.takeWhile(takeP))
    }
  }

  property("make a Process that starts adding elements to a Stream as soon as the given predicate fails") {
   forAll {l: List[Int] =>
     val dropP = (x: Int) => x > 12
     val all: Stream[Int] = Stream(l:_*)
     val p: Process[Int, Int] = Process.dropWhile(dropP)
     val actual = p(all)
     actual.toList should be (l.dropWhile(dropP))
   }
  }

  property("make a Process that emits a running count of the number of elements in a Stream") {
    forAll{l: List[String] =>
      val all = Stream(l:_*)
      val actual = Process.count(all).toList
      val expected = 1 to l.size
      actual should be (expected)
    }
  }

  property("make a Process that emits a running mean of the elements in a Stream") {
    forAll{l: List[Double] =>
      val all = Stream(l:_*)
      val actual = Process.mean(all).toList
      val expected = l.sum/l.size
      if (l.size  == 0) actual should be (empty)
      else if (l.size ==1) actual.head should be (expected)
      else actual(l.size-1) should be (expected)
      actual.size should be (l.size)
    }
  }

  property("make a generic combinator(zip) that lets you express a running mean in terms of sum and count") {
    forAll{l: List[Double] =>
      val all = Stream(l:_*)
      val actual = Process.mean2(all).toList
      val expected = l.sum/l.size
      if (l.size  == 0) actual should be (empty)
      else if (l.size ==1) actual.head should be (expected)
      else actual(l.size-1) should be (expected)
      actual.size should be (l.size)
    }
  }

  property("make exists, a function that returns true as soon as an element of a Stream meets the predicate") {
    forAll{l: List[Int] =>
      val all = Stream(l:_*)
      val positive = (x: Int) => x % 2 == 0
      val actual = Process.exists(positive)(all).toList
      val expected = l.map(x => positive(x))
      actual should be (expected)
    }
  }

  property("use exists and filter out all the false guys using the fusion operator |>") {
    forAll{l: List[Int] =>
      val all = Stream(l:_*)
      val positive = (x: Int) => x % 2 == 0
      val onlyTrue = (p: Boolean) => p == true
      val fused = Process.exists(positive) |> Process.filter(onlyTrue)
      val actual = fused(all).take(1).toList
      val expected = if (l.exists(x => positive(x))) List(true) else List()
      actual should be (expected)
    }
  }

  property("build the original program to tell you whether or a list of strings exceeds a given number of lines by fusing count and exists(and drop and take to cull it)") {
    forAll{l: List[String] =>
      val all = Stream(l:_*)
      val fused: Process[String, Boolean] = Process.countLoop[String] |> Process.exists((x: Int) => x > 10) |> Process.drop(10) |> Process.take(1)
      val actual = fused(all).toList
      if (l.size <= 10)  actual should be (List())
      else actual should be (List(true))
    }
  }

  val createFahrenheitFile = (l: List[Int]) => {
    val file = new java.io.File("fahrenheit.txt")
    val bw = new BufferedWriter(new FileWriter(file, false))
    (1 to 10).foreach((a: Int) => bw.write("#\n"))
    l.foreach(x => bw.write(s"$x\n\n\n"))
    bw.close()
  }

  val createCelsiusFile = (l: String) => {
    val file = new java.io.File("celsius.txt")
    val bw = new BufferedWriter(new FileWriter(file, false))
    bw.write(l)
    bw.close()
  }

  property("build the original program that reads from a file and tells you whether or not the file has greater than some number of lines.  And do it using the IO monad from chapter13.") {
    forAll{l: List[Int] =>
      val gtLines = 10
      val writeFile = (l: List[Int]) => IO {
        val file = new java.io.File("temp.txt")
        val bw = new BufferedWriter(new FileWriter(file, false))
        l.foreach(x => bw.write(s"$x\n"))
        bw.close()
      }

      val actual = writeFile(l).flatMap { _ =>
        val fused: Process[String, Boolean] = Process.countLoop[String] |> Process.exists((x: Int) => x > gtLines)
        FileProcess.processFile(new java.io.File("temp.txt"))(fused)(false)(_ || _)
      }
      (actual.run) should be (l.size > gtLines)
    }
  }

  property("read fahrenheit double values from a file and convert them to celsius and write them to another file") {
    forAll{l: List[Int] =>
      createFahrenheitFile(l)
      val toDouble = (d: String) => d.toDouble
      val toCelsius = (fahrenheit: Double) => (5.0 / 9.0) * (fahrenheit - 32.0)
      val toString = (celsius: Double) => celsius.toString

      val fused = Process.filter((x: String) => ! x.startsWith("#")) |> Process.filter((x: String) => x.trim.nonEmpty) |> Process.lift(toDouble) |> Process.lift(toCelsius) |> Process.lift(toString)
      val actual = FileProcess.processFile(new java.io.File("fahrenheit.txt"))(fused)("")((a, b) => a + "\n" + b)
      createCelsiusFile(actual.run)
    }
  }

  property ("make a Process that sums a Stream using a generic loop") {
    forAll{l: List[Double] =>
      val all = Stream(l:_*)
      val actual = Process.sumLoop(all).toList
      val expected = l.foldLeft(List.empty[Double])((acc, x) => acc match {
        case y :: xs => (x + y) +: acc
        case _ => x +: acc
      }).reverse
      actual.toList should be (expected)
    }
  }

  property("make a Process that emits a running count of the number of elements in a Stream using a generic loop") {
    forAll{l: List[String] =>
      val all = Stream(l:_*)
      val actual = Process.countLoop(all).toList
      val expected = 1 to l.size
      actual should be (expected)
    }
  }

  property("make a Process that emits a running count of the number of elements emitted along with each value. This is also known as zipWithIndex") {
    forAll{l: List[String] =>
      val all = Stream(l:_*)
      val actual = Process.zipWithIndex(all).toList
      val expected = l zip (0 to l.size-1)
      actual should be (expected)
    }
  }

  property("make a fused Process that emits even integers multiplied by 1000") {
    forAll{l: List[Int] =>
      val all = Stream(l:_*)

      val filt  =(x: Int) => x % 2 == 0
      val times1000 = (x: Int) => x * 1000
      val minus3 = (x: Int) => x - 3
      val plus3 = (x: Int) => x + 3
      val expected = l.filter(filt).map(times1000).map(minus3).map(plus3)
      val evenTimes1000 = Process.filter(filt) |> Process.lift(times1000) |> Process.lift(minus3) |> Process.lift(plus3)
      val actual = evenTimes1000(all).take(10).toList
      actual should be (expected.take(10))
    }
  }

  property("make Process a functor that emits even integers multiplied by 1000 with three added to them") {
    forAll{l: List[Int] =>
      val all = Stream(l:_*)
      val filt  =(x: Int) => x % 2 == 0
      val times1000 = (x: Int) => x * 1000
      //val minus3 = (x: Int) => x - 3
      val plus3 = (x: Int) => x + 3
      val expected = l.filter(filt).map(times1000).map(plus3)
      val evenTimes1000 = Process.filter(filt) |> Process.lift(times1000).map(plus3)
      val actual = evenTimes1000(all).take(10).toList
      actual should be (expected.take(10))
    }
  }

  property("make a Process and flatMap it. ***  This is busted. Observe how it it skips the first element whereas map does not.") {
    forAll {ll: List[Int] =>
      val l = List(1,2,3)
      val add12 = (x: Int) => {println("yo"+x);x + 12}
      val all: Stream[Int] = Stream(l:_*)
      val p: Process[Int, Int] = Process.take(1200).map(add12)
      val p2: Process[Int, Int] = Process.take(1200).flatMap(x => Process.lift(add12))
      val actual = p(all).toList
     // val ggg = actual.map(add12)
      println(actual)
      fail("see test name")

     // val add12 = (x: Int) => {println("yo");x + 12}
     // val all: Stream[Int] = Stream(l:_*)
    //  val all2: Stream[Int] = Stream(l:_*)
    //  val p: Process[Int, Int] = Process.lift(add12)

    //  val p2 = Process.take(1200)//.map(x => {println(x);Process.lift(add12)})//x => s"dude--$x"))
    //  val actual = p(all)
    //  println(s"p:${actual.toList}")
    //  println(s"p2:${Process.take(1200)(all2).toList}")
    //  actual.toList should be (l.map(add12))
    }
  }


}
