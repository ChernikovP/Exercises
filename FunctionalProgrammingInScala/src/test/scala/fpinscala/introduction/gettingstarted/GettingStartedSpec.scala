package fpinscala.introduction.gettingstarted

import org.scalacheck.{Gen, Prop}
import org.specs2.ScalaCheck
import org.specs2.mutable.Specification

class GettingStartedSpec extends Specification with ScalaCheck {
  private def concat(a: String, b: String): String = a + b

  private val genStrings: Gen[(String, String)] = for {
    a <- Gen.alphaNumStr
    b <- Gen.alphaNumStr
  } yield a -> b

  private val genArrayOfStrings: Gen[Array[String]] =
    Gen.containerOf[Array, String](Gen.alphaNumStr)

  private val genNonPositiveInt: Gen[Int] =
    Gen.oneOf(Gen.const(0), Gen.negNum[Int])

  "curry / uncurry" in Prop.forAll(genStrings) { case (a, b) =>
      uncurry(curry(concat))(a, b) ==== concat(a, b)
  }

  "isSorted" in Prop.forAll(genArrayOfStrings) { arr =>
    isSorted(arr.sorted)(_ > _) must beTrue
  }

  "fib" should {
    "fib(1) = 0" in { fib(1) ==== 0 }
    "fib(2) = 1" in { fib(2) ==== 1 }
    "fib(3) = 1" in { fib(3) ==== 1 }
    "fib(4) = 2" in { fib(4) ==== 2 }
    "fib(5) = 3" in { fib(5) ==== 3 }
    "fib(6) = 5" in { fib(6) ==== 5 }
    "fib(7) = 8" in { fib(7) ==== 8 }
    "fib(8) = 13" in { fib(8) ==== 13 }
    "fib(9) = 21" in { fib(9) ==== 21 }
    "fib(10) = 34" in { fib(10) ==== 34 }

    "fail if 'i' is negative" in Prop.forAll(genNonPositiveInt) { i =>
      fib(i) must throwA[IllegalArgumentException]
    }
  }
}
