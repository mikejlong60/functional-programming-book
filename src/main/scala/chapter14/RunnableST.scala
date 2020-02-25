package chapter14

trait RunnableST[A] {
  def apply[S]: ST[S, A]
}

object RunnableST {
  def main(args: Array[String]): Unit = {
    val p = new RunnableST[(Int, Int)] {
      def apply[S] = for {
        r1 <- STRef(1)
        r2 <- STRef(2)
        x <- r1.read
        y <- r2.read
        _ <- r1.write(y + 1)
        _ <- r2.write(x + 1)
        a <- r1.read
        b <- r2.read
      } yield (a, b)
    }

    val r = ST.runST(p)
    println(r)

  }
}
