package fpinscala.introduction.datastructures

import org.scalacheck.{Gen, Prop}
import org.specs2.ScalaCheck
import org.specs2.mutable.Specification

import scala.collection.immutable.{List => StdList}

class ListSpec  extends Specification with ScalaCheck {
  private val genSeedAndList: Gen[(String, StdList[String])] = for {
    str <- Gen.alphaNumStr
    lst <- Gen.listOf(Gen.alphaNumStr)
  } yield str -> lst

  "fromStdList / toStdList" in Prop.forAll(Gen.listOf(Gen.alphaNumStr)) { stdLst =>
    List.toStdList(List.fromStdList(stdLst)) ==== stdLst
  }

  "reverse" in Prop.forAll(Gen.listOf(Gen.alphaNumStr)) { stdLst =>
    val lst = List.fromStdList(stdLst)

    List.reverse(List.reverse(lst)) ==== lst
  }

  "foldRight / foldRightViaFoldLeft" in Prop.forAll(genSeedAndList) {
    case (str, stdLst) =>
      val lst = List.fromStdList(stdLst)

      List.foldRight(lst, str)(_ + _) ====
        List.foldRightViaFoldLeft(lst, str)(_ + _)
  }

  "foldLeft / foldLeftViaFoldRight" in Prop.forAll(genSeedAndList) {
    case (str, stdLst) =>
      val lst = List.fromStdList(stdLst)

      List.foldLeft(lst, str)(_ + _) ====
        List.foldLeftViaFoldRight(lst, str)(_ + _)
  }
}
