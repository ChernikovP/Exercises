package fpinscala.introduction.datastructures

import org.scalacheck.{Gen, Prop}
import org.specs2.ScalaCheck
import org.specs2.mutable.Specification

class TreeSpec extends Specification with ScalaCheck {
  private def genLeaf[A](g: Gen[A]) : Gen[Leaf[A]] =
    g.map(Leaf.apply)

  private def genBranch[A](g: Gen[A]) : Gen[Branch[A]] =
    for {
      l <- Gen.sized(h => Gen.resize(h / 2, genTree(g)))
      r <- Gen.sized(h => Gen.resize(h / 2, genTree(g)))
    } yield Branch(l, r)

  private def genTree[A](g: Gen[A]) : Gen[Tree[A]] = Gen.sized { h =>
    if (h <= 0) genLeaf(g)
    else Gen.oneOf(genLeaf(g), genBranch(g))
  }

  "map / mapViaFold" in Prop.forAll(genTree(Gen.alphaNumStr)) { t =>
    Tree.map(t)(_.toUpperCase) === Tree.mapViaFold(t)(_.toUpperCase)
  }
}
