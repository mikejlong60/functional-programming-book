package exercises

object Polymorphism {

  def findFirst(ss: Array[String], key: String): Int = {
    @annotation.tailrec
    def loop(n: Int): Int =
      if (n >= ss.length) -1
      else if (ss(n) == key) n
      else loop(n + 1)
					   
    loop(0)

  }
			
  def findFirstP[A](as: Array[A], p: A => Boolean): Int = {
    @annotation.tailrec
    def loop(n: Int): Int =
      if (n >= as.length) -1
      else if (p(as(n))) n
      else loop(n + 1)
					   
    loop(0)

  }

  val p=  (a: String)  =>
    if (a =="fred") true
    else false
  
}
