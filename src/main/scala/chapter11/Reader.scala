package chapter11

case class Reader[R, A](run: R => A)

