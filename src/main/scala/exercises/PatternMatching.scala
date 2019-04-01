package exercises

object PatternMatching {

  //A total function
  def sum(x: Int, y: Int):Int = x + y

  val sample = 1 to 10
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
    case  x if x > 100000 => "you might be a bigshot"
  }

  def anySalary(x: Int) = x match {
    case  x if x > 100000 => "don't think you are bigshot."
    case _ => "Money will not make you happy"
  }

  val me: PartialFunction[String, String] = {
    case x if x == "Mike Long" => "yup its you"
  }

  //val you: PartialFunction[String, String] = ???
  

}
