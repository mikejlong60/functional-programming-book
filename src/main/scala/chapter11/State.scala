package chapter11

case class State[S, A](run: S => (A, S)) {
  def map[B](f: A => B): State[S, B] =
    State( s => {
      val (a, s1) = run(s)
      (f(a), s1)
    })

  def flatMap[B](f: A => State[S, B] ): State[S, B] =
    State( s => {
      val (a, s1) = run(s)
      f(a).run(s1)
    })
}
