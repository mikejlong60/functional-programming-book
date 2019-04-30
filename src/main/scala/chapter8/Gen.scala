package chapter8

import chapter6.{ RNG, State }


case class Gen[A](sample: State[RNG, A])
