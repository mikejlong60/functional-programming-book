# Functional Programming Course

## Introduction
In the Middle Ages architects built great cathedrals using ad hoc practices. Often in the process of building them they collapsed and lots of people got hurt.  Sometime after the discovery of Calculus architects started building buildings using the laws of mathematics. And the buildings no longer collapsed.  Our work as imperative programmers is somewhat like these Middle Age architects.  It is ad hoc, and for the most part unprincipled, relying on skill and experience instead of science. Last century mathematicians discovered the calculus of Functions. The practice of functional programming is the art of building software using this calculus of functions.

In this course the implementation language will be Scala.  Haskell would be better if I knew it well. But hardly anyone can earn a living doing Haskell and Scala is great.  But if you are better at Javascript you could do the exercises in that language. Rust might work too or untyped languages would probably work as well except I can’t be as helpful to you. Golang, beyond a remedial level will not work because there are not polymorphic functions and the type system does not make it convenient to define Abstract Data Types except with an empty interface and runtime type checking.  Many of the exercises in this course come from a book called _Functional Programming In Scala_ by Runar Bjarnason and Paul Chiusano. 

## Course Syllabus
1. What is Functional Programming?
    1. Programming with pure functions
    2. Recursion
    3. The Closure property of cons
    4. Properties-based testing with Scalacheck
    5. Why types are better than tests
2. What is a Functor?
    1. Algebraic Data Types
    2. Examples from real life?
    3. How is it useful?
    4. The Functor laws
3. What is an Applicative Functor
    1. How is it useful?
    2. Applicative Functor laws
    3. Why Applicative Functors are better than Monads
4. Natural Transformations
    1. What is a Natural Transformatrion -  A mapping between functors.
5. Monoids
    1. Why are they useful?
    2. Monoid laws
6. Monads
    1. When are they useful?
    2. When are they bad?
    3. Monad laws.
7. Lenses - This is the workshop I am presenting at the conference in two weeks.


