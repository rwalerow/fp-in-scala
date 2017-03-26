package rwalerow.chapter3

import org.scalatest.{Matchers, WordSpec}
import rwalerow.chapter3.Exercieses.{drop, dropWhile, length}
import rwalerow.chapter3.List._

class ExercisesSepc extends WordSpec with Matchers {

  "Drop" should {
    "return list itself for n = 0" in {
      val l = List(1, 2, 3)
      drop(l, 0) shouldBe l
    }

    "drop only one" in {
      val l = List(1, 2, 3)
      drop(l, 1) shouldBe List(2, 3)
    }

    "drop all elements" in {
      val l = List(1, 2, 3, 4, 5)
      drop(l, 5) shouldBe Nil
    }
  }

  "DropWhile" should {
    "drop all elements" in {
      val l = List(1,2,3,4,5)
      dropWhile(l, (_: Int) => true) shouldBe Nil
    }
    "drop all smaller then 3" in {
      val l = List(1, 2, 3, 4, 5)
      dropWhile(l, (x: Int) => x < 3) shouldBe List(3, 4, 5)
    }
  }

  "Length" should {
    "be 0" in {
      Exercieses.length(Nil) shouldBe 0
     }

    "be 3" in {
      Exercieses.length(List(1,2,3)) shouldBe 3
    }
  }

  "Sum left" should {
    "10 fo 1,3,6" in {
      sumLeft(List(1,3,6)) shouldBe 10
    }

    "the same as regular sum" in {
      val l = List(1,2,3,4,5,6,7,8,9)
      sumLeft(l) shouldBe sum(l)
    }
  }

  "Product left" should {
    "be 6 for 1,2,3" in {
      productLeft(List(1,2,3)) shouldBe 6
    }

    "be the same for left and regular one" in {
      val l = List(45.0,2,6,2,36,234,6)
      productLeft(l) shouldBe product(l)
    }
  }

  "Reverse" should {
    "reverse nil" in {
      reverse(Nil) shouldBe Nil
    }

    "reverse 1,2,3" in {
      reverse(List(1,2,3)) shouldBe List(3,2,1)
    }
  }

  "Fold rights" should {
    "both should be equal" in {
      foldRight2(List(1,2,3), Nil: List[Int])(Cons(_, _)) shouldBe foldRight(List(1,2,3), Nil: List[Int])(Cons(_, _))
    }
  }

  "Fold lefts" should {
    "both should be equal" in {
      foldLeft2(List(1,2,3), Nil: List[Int])((xs, e) => Cons(e, xs)) shouldBe foldLeft(List(1,2,3), Nil: List[Int])((xs, e) => Cons(e, xs))
    }
  }

}
