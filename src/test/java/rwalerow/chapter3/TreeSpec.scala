package rwalerow.chapter3

import org.scalatest.{Matchers, WordSpec}
import rwalerow.chapter3.Exercieses.maximum
import rwalerow.chapter3.Tree.{depth, depth2, size2}

class TreeSpec extends WordSpec with Matchers {

  "Tree size" should {
    "count regular tree" in {
      Tree.size(Branch(Leaf(10), Leaf(10))) shouldBe 3
      size2(Branch(Leaf(10), Leaf(10))) shouldBe 3
    }

    "leaf as one" in {
      Tree.size(Leaf("lol")) shouldBe 1
      size2(Leaf("lol")) shouldBe 1
    }
  }

  "Maximum" should {
    "find max in small tree" in {
      val tree = Branch(Leaf(10), Leaf(20))

      maximum(tree) shouldBe 20
    }

    "find max in bigger tree" in {
      val tree = Branch(
        Branch(Leaf(3), Leaf(11)),
        Branch(
          Branch(Leaf(30), Leaf(28)),
          Leaf(5)
        )
      )

      maximum(tree) shouldBe 30
    }
  }

  "Tree depth" should {
    "calculate one for leaf" in {
      val t = Leaf(1)

      depth(t) shouldBe 1
      depth2(t) shouldBe 1
    }

    "calculate small tree heigth" in {
      val t = Branch(
        Branch(Leaf(1), Leaf(2)),
        Leaf(10)
      )

      depth(t) shouldBe 3
      depth2(t) shouldBe 3
    }

    "calculate bigger tree" in {
      val t = Branch(
        Branch(
          Branch(
            Branch(
              Leaf(1),
              Branch(
                Branch(Leaf(3), Leaf(35)),
                Leaf(21)
              )
            ),
            Leaf(2)
          ),
          Branch(Leaf(324), Leaf(12))
        ),
        Branch(Leaf(2), Leaf(13))
      )

      depth(t) shouldBe 7
      depth2(t) shouldBe 7
    }
  }

  "tree Map" should {
    "Add 1 to all nodes" in {
      val t = Branch(
        Leaf(1), Leaf(4)
      )

      val expected = Branch(
        Leaf(2), Leaf(5)
      )

      Tree.map(t)(_ + 1) shouldBe expected
      Tree.map2(t)(_ + 1) shouldBe expected
    }

    "Change bigger tree" in {
      val t = Branch(
        Branch(
          Branch(
            Leaf(1),
            Leaf(2)
          ),
          Leaf(3)
        ),
        Leaf(4)
      )

      val expected = Branch(
        Branch(
          Branch(
            Leaf("2"),
            Leaf("3")
          ),
          Leaf("4")
        ),
        Leaf("5")
      )

      val f: Int => String = ((x: Int) => x.toString).compose((x: Int) => x + 1)
      Tree.map(t)(f) shouldBe expected
      Tree.map2(t)(f) shouldBe expected
    }
  }
}
