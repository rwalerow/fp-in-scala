package rwalerow.chapter3

import org.scalatest.{Matchers, WordSpec}
import rwalerow.chapter3.Exercieses.maximum
import rwalerow.chapter3.Tree.size2

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

}
