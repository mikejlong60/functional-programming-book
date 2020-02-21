package chapter13b

import Free._

case class Buffers(in: List[String], out: List[String])
case class ConsoleState[A](run: Buffers=> (A, Buffers)) {
  def getState: ConsoleState[Unit] = ConsoleState(s => ((), s))
  def setState(s: => Buffers): ConsoleState[Unit] = ConsoleState(_ => ((), s))
}

object ConsoleState {
  implicit val monad: chapter11.Monad[ConsoleState] = new chapter11.Monad[ConsoleState] {
    def unit[A](a: => A): ConsoleState[A] = ConsoleState(s => (a, s))
    def flatMap[A, B](st: ConsoleState[A])(f: A => ConsoleState[B]): ConsoleState[B] = ConsoleState( buff => {
      val (a, buff1) = st.run(buff)
      f(a).run(buff1)
    })
  }

  val consoleToState = new (Console ~> ConsoleState) {
    def apply[A](a: Console[A]) = a.toState
  }

  type ConsoleIO[A] = Free.Free[Console, A]

  def runConsoleState[A](io: ConsoleIO[A]): ConsoleState[A] = Free.runFree[Console, ConsoleState, A](io)(consoleToState)

  def main(args: Array[String]): Unit = {
    def printLn(line: String): ConsoleIO[Unit] = Free.Suspend(PrintLine(line))

    val b = Buffers(List("crap"), List("dude"))
    val result = runConsoleState(printLn("yi")).run(b)
    println(result)

    

  }
}
