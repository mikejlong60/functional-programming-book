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
1. Getting set up
    1. Make sure you have a modern JDK installed.
	1. [Install SBT](https://www.scala-sbt.org/)
1. What is Functional Programming?
    1. What does a language need to support functional programming?
	    1. Polymorphic functions
		1. Pattern matching
		1. 1st-class functions
		1. Immutable types
    1. Programming with pure functions
        1. What is a pure function?
		1. What is referential transparency?
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
1. Exercise 1:
    1. Write a fib that uses recursion.  Here is its signature: 
	   ```
	   def fib(n: Long): Long
	   ```
1. Exercise 2:
   1. Write a compose function.  Here is its signature:
       ```
       def compose[A, B, C](f: B => C, g: A => B): A => C
       ```
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


