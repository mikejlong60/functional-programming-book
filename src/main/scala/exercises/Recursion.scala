package exercises

object Recursion {

  def factorial(n: Long): Long = {

    @annotation.tailrec
    def go(n: Long, acc: Long): Long =
      if (n <= 0) acc
      else go(n -1, n * acc)
    

    go(n, 1)
  }


  def fib(n: Long): Long = ???

  def square(n: Long): Long = n * n

  //We will cover function composition and polymorphic functions in a few weeks but this is a teaser for why recursion is better then loops
  //This function makes a new function that applies a to the function g and then takes that result applies it to the function f.
  def compose[A, B, C](f: B => C, g: A => B): A => C = (a: A) => f(g(a))
}
