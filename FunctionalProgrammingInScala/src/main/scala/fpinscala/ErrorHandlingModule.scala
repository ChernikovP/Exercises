package fpinscala

import scala.{None => NativeNone, Option => NativeOption, Some => NativeSome}
import scala.{Either => NativeEither, Left => NativeLeft, Right => NativeRight}

object ErrorHandlingModule {
  sealed trait Option[+A] {
    // Exercise 4.1
    // Implement `map`, `flatMap`, `getOrElse`, `orElse` and `filter`
    def map[B](f: A => B): Option[B] = this match {
      case Some(v) => Some(f(v))
      case None => None
    }

    def flatMap[B](f: A => Option[B]): Option[B] = map(f).getOrElse(None)

    def getOrElse[B >: A](default: => B): B = this match {
      case Some(v) => v
      case None => default
    }

    def orElse[B >: A](default: => Option[B]): Option[B] = map(Some(_)).getOrElse(default)

    def filter(f: A => Boolean): Option[A] = flatMap(v => if (f(v)) Some(v) else None)
  }

  case class Some[+A](value: A) extends Option[A]
  case object None extends Option[Nothing]

  object Option {
    // Exercise 4.3
    // Write a generic function `map2` that combines 2 Option values using
    // binary function
    def map2[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] =
      a.flatMap(aa => b.map(bb => f(aa, bb)))

    // Exercise 4.4
    // Write a function `sequence` that combines a list of Options into one
    // Option containing a list of all the Some values in the original list
    def sequence[A](lst: List[Option[A]]): Option[List[A]] = lst match {
      case h :: t => h.flatMap(hh => sequence(t).map(tt => hh :: tt))
      case Nil => Some(Nil)
    }

    // Exercise 4.5
    // Implement `traverse`, implement `sequence` via `traverse`
    def traverse[A, B](lst: List[A])(f: A => Option[B]): Option[List[B]] = lst match {
      case h :: t => f(h).flatMap(hh => traverse(t)(f).map(tt => hh :: tt))
      case Nil => Some(Nil)
    }

    def sequenceViaTraverse[A](lst: List[Option[A]]): Option[List[A]] =
      traverse(lst)(identity)
  }

  def mean(s: Seq[Double]): Option[Double] =
    if (s.isEmpty) None
    else Some(s.sum / s.length)

  // Exercise 4.2
  // Implement `variance` in terms of `flatMap`
  def variance(s: Seq[Double]): Option[Double] =
    mean(s).flatMap(m => mean(s.map(x => math.pow(x - m, 2))))

  sealed trait Either[+E, +A] {
    // Exercise 4.6
    // Implement `map`, `flatMap`, `orElse` and `map2`
    def map[B](f: A => B): Either[E, B] = this match {
      case l @Left(_) => l
      case Right(v) => Right(f(v))
    }

    def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] = this match {
      case l @Left(_) => l
      case Right(v) => f(v)
    }

    def orElse[EE >: E, B >: A](default: => Either[EE, B]): Either[EE, B] = this match {
      case Left(_) => default
      case r @ Right(_) => r
    }

    def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] =
      this.flatMap(aa => b.map(bb => f(aa, bb)))
  }

  case class Left[+E](value: E) extends Either[E, Nothing]
  case class Right[+A](value: A) extends Either[Nothing, A]

  object Either {
    // Exercise 4.7
    // Implement `sequence` and `traverse` for Either. These methods the first error
    // that's encountered, if there's one
    def traverse[E, A, B](lst: List[A])(f: A => Either[E, B]): Either[E, List[B]] = lst match {
      case h :: t => f(h).flatMap(hh => traverse(t)(f).map(tt => hh :: tt))
      case Nil => Right(Nil)
    }
    def sequence[E, A](lst: List[Either[E, A]]): Either[E, List[A]] = traverse(lst)(identity)
  }

  def run(): Unit = {
    val lst = List(1, 2, 3, 4, 5)

    val square: Int => Option[Int] = x => Some(x * x)
    val parity: Int => Option[Int] = x => if (x % 2 == 0) Some(x) else None

    println(s"List $lst after `traverse` with square function ${Option.traverse(lst)(square)}")
    println(s"List $lst after `traverse` based on parity ${Option.traverse(lst)(parity)}")
  }
}
