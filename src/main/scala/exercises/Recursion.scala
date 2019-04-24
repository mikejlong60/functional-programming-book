package exercises

object Recursion {

  def factorial(n: Int): Int = {

    def go(n: Int, acc: Int): Int =  
      if (n <= 0) acc
      else go(n -1, n * acc)
    

    go(n, 1)
  }

}
