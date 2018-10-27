package fpinscala

import scala.annotation.tailrec

import scala.{Stream => NativeStream}

object StreamModule {
  sealed trait Stream[+A] {
    def headOption: Option[A] = this match {
      case Cons(h, _) => Some(h())
      case Empty => None
    }

    // Exercise 5.1
    // Write a function to convert a Stream to a List
    def toList: List[A] = this match {
      case Cons(h, t) => h() :: t().toList
      case Empty => Nil
    }

    // Exercise 5.2
    // Write a function `take` for returning first n elements of
    // a Stream and `drop` for skipping first n elements
    def take(n: Int): Stream[A] = this match {
      case Cons(h, t) if n > 1 => Stream.cons(h(), t().take(n - 1))
      case Cons(h, _) if n == 1 => Stream.cons(h(), Stream.empty)
      case _ => Stream.empty
    }

    @tailrec
    final def drop(n: Int): Stream[A] = this match {
      case Cons(_, t) if n > 0 => t().drop(n - 1)
      case Cons(_, _) if n == 0 => this
      case _ => Stream.empty
    }

    // Exercise 5.3
    // Write a function `takeWhile`
    def takeWhile(f: A => Boolean): Stream[A] = this match {
      case Cons(h, t) if f(h()) => Stream.cons(h(), t().takeWhile(f))
      case _ => Stream.empty
    }

    @tailrec
    final def exists(p: A => Boolean): Boolean = this match {
      case Cons(h, t) => p(h()) || t().exists(p)
      case Empty => false
    }

    def foldRight[B](z: => B)(f: (A, => B) => B): B = this match {
      case Cons(h, t) => f(h(), t().foldRight(z)(f))
      case Empty => z
    }

    def existsViaFoldRight(p: A => Boolean): Boolean =
      this.foldRight(false)((curr, acc) => p(curr) || acc)

    // Exercise 5.4
    // Implement function `forAll`, which checks that all elements match
    // a given predicate
    def forAll(p: A => Boolean): Boolean = this.foldRight(true)((curr, acc) => p(curr) && acc)

    // Exercise 5.5
    // Implement `takeWhile` using `foldRight`
    def takeWhileViaFoldRight(p: A => Boolean): Stream[A] =
      this.foldRight(Stream.empty[A])((curr, acc) => {
        if (p(curr)) Stream.cons(curr, acc)
        else Stream.empty
      })

    // Exercise 5.6
    // Implement `headOption` using `foldRight`
    def headOptionViaFoldRight: Option[A] =
      this.foldRight(Option.empty[A])((curr, _) => Some(curr))

    // Exercise 5.7
    // Implement `map`, `filter`, `append` and `flatMap` using `foldRight`
    def map[B](f: A => B): Stream[B] =
      this.foldRight(Stream.empty[B])((curr, acc) => Stream.cons(f(curr), acc))

    def filter(p: A => Boolean): Stream[A] =
      this.foldRight(Stream.empty[A])((curr, acc) => {
        if (p(curr)) Stream.cons(curr, acc)
        else acc
      })

    def append[B >: A](that: => Stream[B]): Stream[B] =
      this.foldRight(that)((curr, acc) => Stream.cons(curr, acc))

    def flatMap[B](f: A => Stream[B]): Stream[B] =
      this.foldRight(Stream.empty[B])((curr, acc) => f(curr).append(acc))

    // Exercise 5.13
    // Use `unfold` to implement `map`, `take`, `takeWhile`, `zipWith` and `zipAll`
    def mapViaUnfold[B](f: A => B): Stream[B] = Stream.unfold(this) {
      case Cons(h, t) => Some(f(h()), t())
      case Empty => None
    }

    def takeViaUnfold(n: Int): Stream[A] = Stream.unfold((this, n)) {
      case (Cons(h, t), n) if n > 1 => Some(h(), (t(), n - 1))
      case (Cons(h, _), n) if n == 1 => Some(h(), (Stream.empty, n - 1))
      case _ => None
    }

    def takeWhileViaUnfold(f: A => Boolean): Stream[A] = Stream.unfold(this) {
      case Cons(h, t) if f(h()) => Some(h(), t())
      case _ => None
    }

    def zipWithViaUnfold[B, C](that: Stream[B])(f: (A, B) => C): Stream[C] =
      Stream.unfold((this, that)) {
        case (Cons(ah, at), Cons(bh, bt)) => Some((f(ah(), bh()), (at(), bt())))
        case _ => None
      }

    def zipAll[B](that: Stream[B]): Stream[(Option[A], Option[B])] = Stream.unfold((this, that)) {
      case (Cons(ah, at), Cons(bh, bt)) => Some((Some(ah()), Some(bh())), (at(), bt()))
      case (Empty, Cons(bh, bt)) => Some((None, Some(bh())), (Empty, bt()))
      case (Cons(ah, at), Empty) => Some((Some(ah()), None), (at(), Empty))
      case (Empty, Empty) => None
    }

    // Exercise 5.14
    // Implement a function `startsWith`
    def startsWith[A](that: Stream[A]): Boolean = this.zipAll(that)
      .takeWhile{ case (l, r) => r.isDefined }
      .forAll{ case (l, r) => l == r }

    // Exercise 5.15
    // Implement `tails` using `unfold`
    def tails: Stream[Stream[A]] = Stream.unfold(this) {
      case s @ Cons(_, t) => Some(s, t())
      case Empty => None
    }

    def hasSubsequence[A](that: Stream[A]): Boolean = this.tails.exists(_.startsWith(that))

    // Exercise 5.16
    // Generalize `tails` to the function `scanRight`, which is like `foldRight`
    // that returns a Stream of intermediate results
    def scanRight[B](z: => B)(f: (A, => B) => B): Stream[B] =
      this.foldRight((z, Stream(z)))((curr, acc) => {
        val (currAcc, currResults) = acc
        val newAcc = f(curr, currAcc)

        (newAcc, Stream.cons(newAcc, currResults))
      })._2
  }

  case class Cons[+A](head: () => A, tail: () => Stream[A]) extends Stream[A]
  case object Empty extends Stream[Nothing]

  object Stream {
    def cons[A](h: => A, t: => Stream[A]): Stream[A] = {
      lazy val head = h
      lazy val tail = t

      Cons(() => head, () => tail)
    }

    def empty[A]: Stream[A] = Empty

    def apply[A](seq: A*): Stream[A] =
      if (seq.isEmpty) empty
      else cons(seq.head, apply(seq.tail: _*))

    val ones: Stream[Int] = cons(1, ones)

    // Exercise 5.8
    // Implement a function `constant`, which returns an infinite Stream
    // of a given constant
    def constant[A](a: A): Stream[A] = cons(a, constant(a))

    // Exercise 5.9
    // Write a function `from`, which returns an infinite Stream of integers
    // starting from a given `n`
    def from(n: Int): Stream[Int] = cons(n, from(n + 1))

    // Exercise 5.9
    // Write a function `fibs`, which returns an infinite Stream of Fibonacci
    // numbers
    def fibs: Stream[Int] = {
      def loop(prev: Int, curr: Int): Stream[Int] =
        cons(prev, loop(curr, prev + curr))

      loop(0, 1)
    }

    // Exercise 5.11
    // Write a function `unfold`, which takes an initial state and a function
    // for producing both the next state and the next value in the Stream
    def unfold[A, S](s: S)(f: S => Option[(A, S)]): Stream[A] = f(s) match {
      case Some((na, ns)) => cons(na, unfold(ns)(f))
      case None => empty
    }

    // Exercise 5.12
    // Write `constant`, `from`, `fibs` and `ones` via `unfold`
    def constantViaUnfold[A](a: A): Stream[A] = unfold(a)(s => Some((s, s)))

    def fromViaUnfold(n: Int): Stream[Int] = unfold(n)(s => Some((s, s + 1)))

    def fibsViaUnfold: Stream[Int] =
      unfold((0, 1)){ case (prev, curr) => Some((prev, (curr, prev + curr)))}

    val onesViaUnfold: Stream[Int] = constantViaUnfold(1)
  }

  def run: Unit = {
    // Evaluating expression multiple times
    println("Wrong initialization!")
    val badX = Cons(() => { println("Intensive Computations..."); 42 }, () => Empty)
    println(badX.headOption)
    println(badX.headOption)

    // Evaluating expression only once
    println("Correct initialization!")
    val goodX = Stream.cons({ println("Intensive Computations..."); 42 }, Empty)
    println(goodX.headOption)
    println(goodX.headOption)

    val sup = Stream(1, 2, 3, 4, 5)
    val sub = Stream(2, 3, 4)

    println("Trace execution:")
    val l = sup.map(x => { println(s"'map' on $x"); x + 10 })
      .filter(x => { println(s"'filter' on $x"); x % 2 == 0 })
      .toList
    println(s"Final list - $l")


    println(s"Zipping Stream(1, 2, 3, 4, 5) with Stream(2, 3, 4) - ${sup.zipAll(sub).toList}")

    println(s"Tails of Stream(1, 2, 3, 4, 5) - ${sup.tails.toList.map(_.toList)}")

    println(s"Stream(1, 2, 3, 4, 5) contains Stream(2, 3, 4) - ${sup.hasSubsequence(sub)}")
  }
}
