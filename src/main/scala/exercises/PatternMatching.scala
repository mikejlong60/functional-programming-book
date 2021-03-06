package exercises

object PatternMatching {

  //A total function
  def sum(x: Int, y: Int):Int = x + y

  val sample = 1 to 10

  val isBiggerThan5: PartialFunction[Int, String] = {
    case x if x > 5 => "is bigger than five"
  }

  //From https://www.scala-lang.org/api/current/scala/PartialFunction.html
  val isEven: PartialFunction[Int, String] = {
    case x if x % 2 == 0 => x+" is even"
  }

  // the method collect uses isDefinedAt to select which members to collect
  val evenNumbers = sample collect isEven

  val isOdd: PartialFunction[Int, String] = {
    case x if x % 2 == 1 => x+" is odd"
  }

  // the method orElse allows chaining another partial function to handle
  // input outside the declared domain
  val numbers = sample map (isEven orElse isOdd)

  def aBigSalary(x: Int) = x match {
    case  x if x >= 100000 => "you might be a bigshot"
  }

  def anySalary(x: Int) = x match {
    case  x if x < 100000 => "don't think you are bigshot."
    case _ => "Money will not make you happy"
  }

  val me: PartialFunction[String, String] = {
    case x if x == "Mike Long" => "yup its you"
  }

  //val you: PartialFunction[String, String] = ???
  
  def isItMike(whom: String): Boolean = whom match {
    case "mike" => true
    case "Hiromi" => false
    case "Daniel" => false
    case "David" => false
    case "Seth" => false
    case "Luther" => false
    case _ => false
  }

  val aBigList = (1 to 500).toList
  def sum1stThree(theList: List[Int]): Int = theList match {
    case x :: y :: z :: xs => x + y + z
    case _ => 0
  }


}
