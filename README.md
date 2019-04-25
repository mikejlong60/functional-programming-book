# Functional Programming Course

## Introduction
In the Middle Ages architects built great cathedrals using ad hoc practices. Often in the process of building them they collapsed and lots of people got hurt.  Sometime after the discovery of Calculus architects started building buildings using the laws of mathematics. And the buildings no longer collapsed.  Our work as imperative programmers is somewhat like these Middle Age architects.  It is ad hoc, and for the most part unprincipled, relying on skill and experience instead of science. Last century mathematicians discovered the calculus of Functions. The practice of functional programming is the art of building software using this calculus of functions.

In this course the implementation language will be Scala.  Haskell would be better if I knew it well. But hardly anyone can earn a living doing Haskell and Scala is great.  But if you are better at Javascript you could do the exercises in that language. Rust might work too or untyped languages would probably work as well except I can’t be as helpful to you. Golang, beyond a remedial level will not work because there are not polymorphic functions and the type system does not make it convenient to define Abstract Data Types except with an empty interface and runtime type checking.  Many of the exercises in this course come from a book called _Functional Programming In Scala_ by Runar Bjarnason and Paul Chiusano. 

1. What are the benefits of functional programming?
    1. Programs can be distributed without fear of side effects.  Distribution means across threads/channels and across processes and/or machines.
	2. You achieve much greater reuse than you do in imperative programming. See examples from Chapter 5 exercises.
	3. Applications are smaller.
	4. Applications are cheaper to maintain.
	5. Applications are easier to understand.
	6. Its way more fun than imperative programming.  Its fun telling your boss "I took 2 weeks to do this thing that would have taken a very good imperative programmer 10 weeks and I only needed 50 lines of code".
1. If all that it true then why isn't it more popular?
    1. Because it's harder than imperative programming.
	2. It takes a long time to learn.
	3. It makes us feel stupid.
	4. It looks weird.
	5. Because Google doesn't use it.

## Course Syllabus
1. Week 0 - Getting set up and trying to convince you to stick it out.
    1. Make sure you have a modern JDK installed.
	1. [Install SBT](https://www.scala-sbt.org/). For the initial weeks we will not need an editor at all. We will be using SBT and its repl for the exercises.
1. What is Functional Programming?
    1. Fundamental Characteristics of Functional Programming
		1. Immutable types
		1. Pattern matching
		1. Learning to use recursion instead of loops.
		1. Polymorphic functions
		1. 1st-class functions
		1. Laziness
		1. Tail recursion
	1. Laying a Foundation.  For at least the first few weeks you will work on gaining a fundamental understanding of the preceding ideas.  We will not need an editor or an IDE:
		1. Week 0 - Getting set up and trying to convince you to stick it out.
			1. Make sure you have a modern JDK installed.
			1. [Install SBT](https://www.scala-sbt.org/). For the initial weeks we will not need an editor at all. We will be using SBT and its repl for the exercises.  You will have completed week 0 when you have done the following: Installed SBT with `brew install sbt`.  Cloned this repo someplace with: `git clone https://github.com/mikejlong60/functional-programming-book.git`.   Descend into the preceding project's directory in a shell and type `sbt`.  Then when in sbt  type: `test`.  A whole lot of stuff gets downloaded and you will see happy green tests.
			1. SBT paste mode: 
			```
			scala> :paste
			// Entering paste mode (ctrl-D to finish)

			```
		1. Week 1 - What is a case class? A constructor for an immutable type? Or is it a function? Tell me why?
			1. Exercise 1: Make a case class
			1. Exercise 2: Make a case class that references another case class
			1. Exercise 3: Learning to compare case classes.  Make two instances of your case class above.  How can you compare it for equality.  
		1. Week 2, 3 - Pattern Matching
			1. For at least the next week or two we will work at understanding pattern matching and the related concepts of partial and total functions:
				1. What is a total function? A total function is a function that is defined for all possible values of its input. That is, it always terminates and always returns a value. 
				```
				def sum(x: Int, y: Int):Int = x + y
				```
				1. Contrasting a Partial Function with a Partially Applied Function?   A partially applied function is a very different animal.  A partially applied function is a function value that has one or more of its arguments applied.  Its a way making a new function with some parameter fixed.  Say for example that you have a two parameter function that gets called very often. And that one of its parameters is fixed and the other is not. An example from Notification Service was a Mustache template that needed to get compiled one time and was then fixed for the life of the application.  That computation is expensive. So I partially applied it.  Here is an example:
				```
				val divide = (num: Double, den: Double) => num / den

				val halfOf: (Double) => Double = divide(_, 2)
				halfOf 20 == 10
				```
				1. What is a partial function? From https://www.scala-lang.org/api/current/scala/PartialFunction.html
				```
				trait PartialFunction[-A, +B] extends (A) ⇒ B
				``` 
				Unlike a total function, a partial function might not apply for all possible values of its domain(its possible inputs). Everything in FP returns a value. Case and match is no different.  This is different than if/else/ switch.  A value passed into a case statement may not have any matches. The compiler might warn you when this is the case(not in the console). And you will always get a runtime error if this is the case.  A Case expression is a Partial Function. They can be combined with the Match keyword.  But its just as accurate to make them as Partial Functions and combine them with orElse.  Partial functions have a fantastic property. They are composable.  And therein lies their power.  Everything always comes back to the composition of functions.  Such is the case with ```collect``` in the exercises.  You can apply a partial function just like any other function.  You can also ask it if your particular argument would apply using isDefinedAt.
				1. Why does this relate to pattern matching?  Because I don't want you to think about branching like an imperative programmer.  I want you to think in terms of functions. At the surface case and match seem like their imperative counterparts but they are not that similar.  
				1. For these exercises use the console.  And add your functions to the PatternMatching object in the exercises package.  Or you can use the console directly.  To start out in the console:
			    1. sbt
					1. console
					1. import exercises.PatternMatching._
					1. I am going to take you on a tour of partial functions and pattern matching.
						1. Use a partial function on a list of numbers to see which are even, are odd, or both
						1. Create a partially applied function such as the one above so you will understand what it is and not be confused about the difference between a partial function and a partially applied function.
						1. Show how partial functions are composable using ```orElse```.
						1. Use isDefinedAt to see if you will get a match error at runtime. 
						1. Make Scala throw a Match Error on a  partial function
						1. Partial Functions -  Implement you.
						1. Make it throw a Match Error
						1. Compose it with me.
						1. Use isDefinedAt to see if you will get a Match Error.
						1. Use the collect on a Scala range statement ```1 to 50``` to see which are odd and which are even.  See the example.
						1. Understand the importance of the ordering of partial functions. Use the previous range to compose three partial functions ```isEven isOdd isBiggerThan5``` with ```orElse```. 
						1. Pattern match on a list to add its 1st 4 elements if it has 4. If it does not return 0.
						1. Understand pattern matching on strings.  Explore many nuances.
        1. Week 3 - Recursion 
		   1. What is recursion? - Read this: https://www.geeksforgeeks.org/recursion/
			   1. Important points:
				   1. Every recursive function can be written non-recursively as a loop.  We will go back and forth in some exercises.
				   1. What is great about recursion?
					   1. It's beautiful, like a Chambered Nautilus or an M.C Escher painting or counterpoint in the music of Bach.
					   1. Its composable. A recursive function can be composed with other functions. Try doing that with a loop. 
						   1. Notes for Mike. Show example of function composition using functions that are recursive. Show that they can be composed
				   1. What's bad about recursion?
					   1. It can't operate in constant stack space without tail recursion. Golang does not support tail-recursion.
					   1. Some recursive functions are not tail recursive like foldRight on a List.  But you can do a clever trick to make foldRight tail recursive.
				   1. What is tail recursion? 
					   1. A function is tail recursive when the recursive call is the last thing in the function. In that case the compiler can optimize it as a loop and does not need to keep the stack frame around, the parameters to the function contain all necessary state to execute the function call.

				   1. Exercises:
					   1. Implement factorial from  https://github.com/mikejlong60/functional-programming-book/blob/master/src/main/scala/exercises/Recursion.scala using a loop.  I don't want you to use loops in FP code but you need to begin building an understanding of how loops and recursion are related.
					   1. Note the technique of using an inner function from the previous exercise.  Use that technique to implement a function that returns a given Fibonacci number.  See the preceding file for that function's signature.
		1. Week 4 - Building an understanding of polymorphic functions
		1. Week 5 - 1st class functions
		1. Week 6 - laziness
		1. Week 7 - tail recursion
1. Stuff we will start talking about in a couple months.  
		1. What is referential transparency?  Something is referentially transparent if it cannot be observed from the outside. Examples include local variables, writing to a hidden file(might cause a panic and crash with out od disk space but it's still transparent because no caller could observe this or affect the outcome).
		1. What is the substitution model?	
    1. Recursion
	    1. Why should you do it?
		    1. It's beautiful:
			```
			@annotation.tailrec
            def foldLeft[A, B](as: List[A], z: B)(f: (B, A) => B): B = as match {
              case Cons(x, xs) => foldLeft(xs, f(z, x))(f)
              case _ => z
            }

			```
		1. Recursion is closely related to the _Closure property of cons_.  A function has this property when applying a function to the members of some set produces an element that is again a member of this set.  The function ```f``` above has this property because it both takes and produces a B.  Functions that have this property can be composed to produce new functions.  And these functions have algebaric properties.  This is the big deal of FP because it allows us to reuse functions.  Its what I did to generalize the addition of Kafka logging for the suite of Notifications applications.
    1. Properties-based testing with Scalacheck
    1. Why types are better than tests. In many cases the type system tells you whether or not your function is correct. Often there is only one possible implementation.
	1. What does the term `currying` mean?
	1. What does the term `partial application of a function` mean?
	1. What does the term `partial function` mean?
	1. What is a Functor?
		1. Algebraic Data Types
		2. Examples from real life?
		3. How is it useful?
		4. The Functor laws
	1. What is an Applicative Functor
		1. How is it useful?
		2. Applicative Functor laws
		3. Why Applicative Functors are better than Monads
	1. Natural Transformations
		1. What is a Natural Transformatrion -  A mapping between functors.
	1. Monoids
		1. Why are they useful?
		2. Monoid laws
	1. Monads
		1. When are they useful?
		2. When are they bad?
		3. Monad laws.
	1. Lenses - This is the workshop I am presenting at the conference in two weeks.


