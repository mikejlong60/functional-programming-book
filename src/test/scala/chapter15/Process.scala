package chapter15

sealed trait Process[I, O]

case class Await[I, O](recv: Option[I] => Process[I, O]) extends Process[I, O]
case class Halt[I, O]() extends Process[I, O]
case class Emit[I, O] (head: O, tail: Process[I, O] = Halt[I, O]()) extends Process[I, O]
