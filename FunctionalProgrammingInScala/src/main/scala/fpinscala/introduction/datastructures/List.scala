package fpinscala.introduction.datastructures

import scala.annotation.tailrec
import scala.collection.immutable.{List => StdList, Nil => StdNil}


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
    case Nil =>
      throw new IllegalArgumentException(
        "Could not extract tail of empty list"
      )
  }

  // Exercise 3.3
  // Implement the function for replacing the first element of a List
  // with a different value
  def setHead[A](lst: List[A], nh: A): List[A] = lst match {
    case Cons(_, t) => Cons(nh, t)
    case Nil =>
      throw new IllegalArgumentException(
        "Could not replace head of empty list"
      )
  }

  // Exercise 3.4
  // Generalize `tail` to function that removes the first `n` elements
  // from a list
  @tailrec
  def drop[A](lst: List[A], n: Int): List[A] =
    if (n <= 0) lst
    else lst match {
      case Cons(_, t) => drop(t, n - 1)
      case Nil => Nil
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
    case Cons(_, Nil) => Nil
    case Cons(h, t) => Cons(h, init(t))
    case Nil =>
      throw new IllegalArgumentException(
        "Could not drop last element of empty list"
      )
  }

  def foldRight[A, B](lst: List[A], z: B)(f: (A, B) => B): B = lst match {
    case Cons(h, t) => f(h, foldRight(t, z)(f))
    case Nil => z
  }

  // Exercise 3.10
  // Implement `foldLeft` using tail recursion
  @tailrec
  def foldLeft[A, B](lst: List[A], z: B)(f: (B, A) => B): B = lst match {
    case Cons(h, t) => foldLeft(t, f(z, h))(f)
    case Nil => z
  }

  // Exercise 3.9 & 3.11
  // Compute the length of a List using `foldRight` and `foldLeft`
  def lengthViaFoldRight[A](lst: List[A]): Int =
    foldRight(lst, 0)((_, acc) => acc + 1)
  def lengthViaFoldLeft[A](lst: List[A]): Int =
    foldLeft(lst, 0)((acc, _) => acc + 1)

  // Exercise 3.12
  // Implement function that reverses a List using `fold`
  def reverse[A](lst: List[A]): List[A] =
    foldLeft(lst, Nil: List[A])((acc, curr) => Cons(curr, acc))

  // Exercise 3.13
  // Implement `foldRight` using `foldLeft` and vice versa
  def foldRightViaFoldLeft[A, B](lst: List[A], z: B)(f: (A, B) => B): B =
    foldLeft[A, B => B](lst, identity)((accFunc, a) =>
      b => accFunc(f(a, b))
    )(z)
  def foldLeftViaFoldRight[A, B](lst: List[A], z: B)(f: (B, A) => B): B =
    foldRight[A, B => B](lst, identity)((a, accFunc) =>
      b => accFunc(f(b, a))
    )(z)

  // Exercise 3.14
  // Implement `append` in terms of `foldLeft` or `foldRight`
  def appendViaFoldRight[A](l: List[A], r: List[A]): List[A] =
    foldRight(l, r)(Cons(_, _))

  // Exercise 3.15
  // Implement a function that concatenates a list of lists
  def concat[A](lst: List[List[A]]): List[A] =
    foldRight(lst, Nil: List[A])(append)

  // Exercise 3.18
  // Write a function `map` that generalizes modifying each element
  // in a list, while maintaining the structure of the list
  def map[A, B](lst: List[A])(f: A => B): List[B] =
    foldRight(lst, Nil: List[B])((a, acc) => Cons(f(a), acc))

  // Exercise 3.19
  // Write a function `filter` that removes elements from a list,
  // unless they satisfy a given predicate
  def filter[A](lst: List[A])(p: A => Boolean): List[A] =
    foldRight(lst, Nil: List[A])((a, acc) =>
      if (p(a)) Cons(a, acc)
      else acc
    )

  // Exercise 3.20
  // Write a function `flatMap` that works like `map`, except the
  // function will return a list instead of single element
  def flatMap[A, B](lst: List[A])(f: A => List[B]): List[B] =
    foldRight(lst, Nil: List[B])((a, acc) => append(f(a), acc))

  // Exercise 3.21
  // Implement `filter` using `flatMap`
  def filterViaFlatMap[A](lst: List[A])(p: A => Boolean): List[A] =
    flatMap(lst)(a => if (p(a)) List(a) else Nil)

  // Exercise 3.23
  // Implement `zipWith`
  def zipWith[A, B, C](a: List[A], b: List[B])(f: (A, B) => C): List[C] = {
    @tailrec
    def loop(la: List[A], lb: List[B], acc: List[C]): List[C] =
      la -> lb match {
        case (Cons(ha, ta), Cons(hb, tb)) =>
          loop(ta, tb, Cons(f(ha, hb), acc))
        case _ => acc
      }

    reverse(loop(a, b, Nil: List[C]))
  }

  // Exercise 3.24
  // Implement `hasSubsequence` for checking whether a List contains
  // another List as a subsequence
  @tailrec
  def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean = {
    @tailrec
    def startsWith(a: List[A], b: List[A]): Boolean =
      (a, b) match {
        case (Cons(ah, at), Cons(bh, bt)) if ah == bh =>
          startsWith(at, bt)
        case (_, Nil) => true
        case _ => false
      }

    sup match {
      case Cons(_, _) if startsWith(sup, sub) => true
      case Cons(_, t) => hasSubsequence(t, sub)
      case Nil => sub == Nil
    }
  }

  def toStdList[A](lst: List[A]): StdList[A] =
    foldRight(lst, StdNil: StdList[A])(_ :: _)

  def fromStdList[A](lst: StdList[A]): List[A] =
    lst.foldRight(Nil: List[A])(Cons(_, _))
}
