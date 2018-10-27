package fpinscala.introduction

import scala.annotation.tailrec

package object gettingstarted {
  def factorial(n: Int): Int = {
    @tailrec
    def loop(i: Int, acc: Int): Int =
      if (i == 0) acc
      else loop(i - 1, acc * i)

    if (n <= 0)
      throw new IllegalArgumentException("Only positive integers are accepted")
    else loop(n, 1)
  }

  // Exercise 2.1
  // Write a recursive function to get the nth Fibonacci number
  def fib(n: Int): Int = {
    @tailrec
    def loop(i: Int, curr: Int, next: Int): Int = {
      if (i == 1) curr
      else loop(i - 1, next, curr + next)
    }

    if (n <= 0)
      throw new IllegalArgumentException("Only positive integers are accepted")
    else loop(n, 0, 1)
  }

  // Exercise 2.2
  // Implement method that checks whether an Array is sorted according to
  // given comparison function
  def isSorted[A](arr: Array[A])(greater: (A, A) => Boolean): Boolean = {
    @tailrec
    def loop(i: Int): Boolean = {
      if (i <= 0) true
      else if (greater(arr(i - 1), arr(i))) false
      else loop(i - 1)
    }

    loop(arr.length - 1)
  }

  def partial[A,B,C](f: (A,B) => C, a: A): B => C =
    b => f(a, b)

  // Exercise 2.3
  // Implement 'curry'
  def curry[A, B, C](f: (A, B) => C): A => B => C =
    a => b => f(a, b)

  // Exercise 2.4
  // Implement 'uncurry'
  def uncurry[A, B, C](f: A => B => C): (A, B) => C =
    (a, b) => f(a)(b)

  // Exercise 2.5
  // Implement a higher-order function that composes two functions
  def compose[A, B, C](f: B => C, g: A => B): A => C =
    g andThen f
}
