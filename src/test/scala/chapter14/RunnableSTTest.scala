package chapter14

import org.scalatest.prop.PropertyChecks
import org.scalatest.{Matchers, PropSpec}

class RunnableSTTest extends PropSpec with PropertyChecks with Matchers {

  property("test running first program to produce something") {
    forAll {(x:  Int, y: Int) =>
      val swapper: RunnableST[(Int, Int)] = new RunnableST[(Int, Int)] {
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

      val actual: (Int, Int) = ST.runST(swapper)
      actual should be ((y + 1, x + 1))
    }
  }

  /**
  property("test function that fills STArray from map") {
    forAll {mm:  Map[Int, String] =>
      whenever (mm.size > 0) {
        val m = Map(0 -> "zero", 10 -> "zero", 13 -> "- twelve")
//      val toArray: RunnableST[Array[String]] = new RunnableST[Array[String]] {
      val toArray  = new RunnableST[(Map[Int, String], Array[String])] {
        def apply[S]: ST[S, (Map[Int, String], Array[String])] = for {
          r1 <- STRef(m)
          m <- r1.read
          aa = Array.fill(m.keys.max)("initial")
          bb = m.keys.map(k => aa(k-1) = m(k))
          r2 <- STRef(aa)
          z <- r1.read
          b <- r2.read
        } yield (z, b)
      }

        val toArray2  = new RunnableST[(Map[Int, String], Array[String])] {
          def apply[S]: ST[S, (Map[Int, String], Array[String])] = {
            val a = STRef(m)
            val b  = a.flatMap(r1 => {
              r1.read.flatMap(m => {
                val aa = Array.fill(m.keys.max)("initial")
                val bb = m.keys.map(k => aa(k -1) = m(k))
                STRef(aa).map(r2 => r2.read)///flatMap(z => z.read))
              })

            })
            ???
          }
 




      val actual = ST.runST(toArray)
      println("fred:"+actual._2.toList)

      val act2: ST[Nothing, STArray[Nothing, String]] = STArray(12, "a")
     // val gggg = new RunnableST[Nothing, STArray[Nothing, String]] {
      //  def apply[S]: ST[S, (Map[Int, String], List[String])]
      //}
      //ST.runST(act2)
    //  actual should be ((y + 1, x + 1))
      }
    }
  }

    */
  property ("Verify that RunnableST with a wildcard type will not typecheck. ") {
    """
             val ref = ST.runST(new RunnableST[STRef[_, Int]]) {
               def apply[S] = for {
                 r1 <- STRef(1)
               } yield r1 
             }
""" shouldNot typeCheck
  }

}
