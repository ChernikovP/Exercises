package fpinscala.modules

import scala.annotation.tailrec

object DataStructuresModule {
  sealed trait List[+A]

  case object Nil extends List[Nothing]
  case class Cons[+A](head: A, tail: List[A]) extends List[A]

  object List {
    def apply[A](seq: A*): List[A] =
      if (seq.isEmpty) Nil
      else Cons(seq.head, apply(seq.tail: _*))

    // Exercise 3.2
    // Implement the function for removing the first element of a List
    def tail[A](lst: List[A]): List[A] = lst match {
      case Cons(_, t) => t
      case Nil => throw new IllegalArgumentException("Could not extract tail of empty list")
    }

    // Exercise 3.3
    // Implement the function for replacing the first element of a List
    // with a different value
    def setHead[A](lst: List[A], nh: A): List[A] = lst match {
      case Cons(oh, t) => Cons(nh, t)
      case Nil => throw new IllegalArgumentException("Could not replace head of empty list")
    }

    // Exercise 3.4
    // Generalize `tail` to function that removes the first `n` elements from
    // a list
    @tailrec
    def drop[A](lst: List[A], n: Int): List[A] =
      if (n <= 0) lst
      else lst match {
        case Cons(_, t) => drop(t, n - 1)
        case Nil => throw new IllegalArgumentException("Could not drop head of empty list")
      }

    // Exercise 3.5
    // Implement the function, which removes elements from the List as long
    // as they match the predicate
    @tailrec
    def dropWhile[A](lst: List[A], p: A => Boolean): List[A] = lst match {
      case Cons(h, t) if p(h) => dropWhile(t, p)
      case _ => lst
    }

    def append[A](a: List[A], b: List[A]): List[A] = a match {
      case Cons(ah, at) => Cons(ah, append(at, b))
      case Nil => b
    }

    // Exercise 3.6
    // Implement the function, that returns a List without last element
    def init[A](lst: List[A]): List[A] = lst match {
      case Cons(h, t @ Cons(_, _)) => Cons(h, init(t))
      case Cons(_, Nil) => Nil
      case Nil => throw new IllegalArgumentException("Could not drop last element of empty list")
    }

    def foldRight[A, B](lst: List[A], z: B)(f: (A, B) => B): B = lst match {
      case Cons(h, t) => f(h, foldRight(t, z)(f))
      case Nil => z
    }

    // Exercise 3.10
    // Implement `foldLeft` using tail recursion
    @tailrec
    def foldLeft[A, B](lst: List[A], z: B)(f: (B, A) => B): B = lst match {
      case Nil => z
      case Cons(h, t) => foldLeft(t, f(z, h))(f)
    }

    // Exercise 3.9 & 3.11
    // Compute the length of a List using `foldRight` and `foldLeft`
    def lengthFR[A](lst: List[A]): Int = foldRight(lst, 0)((_, acc) => acc + 1)
    def lengthFL[A](lst: List[A]): Int = foldLeft(lst, 0)((acc, _) => acc + 1)

    // Exercise 3.12
    // Implement function that reverses a List using `fold`
    def reverse[A](lst: List[A]): List[A] =
      foldLeft(lst, Nil: List[A])((acc, curr) => Cons(curr, acc))

    // Exercise 3.13
    // Implement `foldRight` using `foldLeft` and vice versa
    def foldLeftViaFoldRight[A, B](lst: List[A], z: B)(f: (B, A) => B): B =
      foldRight[A, B => B](lst, identity)((a, bFunc) => x => bFunc(f(x, a)))(z)
    def foldRightViaFoldLeft[A, B](lst: List[A], z: B)(f: (A, B) => B): B =
      foldLeft[A, B => B](lst, identity)((bFunc, a) => x => bFunc(f(a, x)))(z)

    // Exercise 3.14
    // Implement `append` in terms of `foldLeft` or `foldRight`
    def appendFR[A](l: List[A], r: List[A]): List[A] =
      foldRight(l, r)(Cons(_, _))

    // Exercise 3.15
    // Implement a function that concatenates a list of lists
    def concat[A](lst: List[List[A]]): List[A] =
      foldRight(lst, Nil: List[A])(append)

    // Exercise 3.18
    // Write a function `map` that generalizes modifying each element in a list
    // while maintaining the structure of the list
    def map[A, B](lst: List[A])(f: A => B): List[B] =
      foldRight(lst, Nil: List[B])((curr, acc) => Cons(f(curr), acc))

    // Exercise 3.19
    // Write a function `filter` that removes elements from a list unless they
    // satisfy a given predicate
    def filter[A](lst: List[A])(p: A => Boolean): List[A] =
      foldRight(lst, Nil: List[A])((curr, acc) => if (p(curr)) Cons(curr, acc) else acc)

    // Exercise 3.20
    // Write a function `flatMap` that works like `map` except the function
    // will return a list instead of single element
    def flatMap[A, B](lst: List[A])(f: A => List[B]): List[B] =
      foldRight(lst, Nil: List[B])((curr, acc) => append(f(curr), acc))

    // Exercise 3.21
    // Implement `filter` using `flatMap`
    def filterFlatMap[A](lst: List[A])(f: A => Boolean): List[A] =
      flatMap(lst)(e => if (f(e)) List(e) else Nil)

    // Exercise 3.23
    // Implement `zipWith`
    def zipWith[A, B, C](a: List[A], b: List[B])(f: (A, B) => C): List[C] = {
      @tailrec
      def loop(l: List[A], r: List[B], acc: List[C]): List[C] = (l, r) match {
        case (_, Nil) => acc
        case (Nil, _) => acc
        case (Cons(lh, lt), Cons(rh, rt)) => loop(lt, rt, Cons(f(lh, rh), acc))
      }

      reverse(loop(a, b, Nil: List[C]))
    }
  }

  def run: Unit = {
    val lst = List(1, 2, 3)

    println(s"List $lst folding via `foldLeft` with Cons = " +
      s"${List.foldLeft(lst, Nil: List[Int])((a, b) => Cons(b, a))}")
    println(s"List $lst folding via `foldRight` with Cons = " +
      s"${List.foldRight(lst, Nil: List[Int])(Cons(_, _))}")
    println(s"List $lst folding via `foldLeftViaFoldRight` with Cons = " +
      s"${List.foldLeftViaFoldRight(lst, Nil: List[Int])((a, b) => Cons(b, a))}")
    println(s"List $lst folding via `foldRightViaFoldLeft` with Cons = " +
      s"${List.foldRightViaFoldLeft(lst, Nil: List[Int])(Cons(_, _))}")
  }
}