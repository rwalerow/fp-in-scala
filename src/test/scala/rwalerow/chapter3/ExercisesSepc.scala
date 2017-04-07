package rwalerow.chapter3

import org.scalatest.{Matchers, WordSpec}
import rwalerow.chapter3.Exercieses._
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

  "AppendFold" should {
    "correctly append Nil, List(1,2)" in {
      appendFold(Nil, List(1,2)) shouldBe List(1,2)
    }

    "correctly append List(1,2), Nil" in {
      appendFold(List(1,2), Nil) shouldBe List(1,2)
    }

    "correctly append List(1,2), List(4,3)" in {
      appendFold(List(1,2), List(4,3)) shouldBe List(1,2,4,3)
    }
  }

  "Concatenate" should {
    "correctly concatenate 2 lists" in {
      concatenate(List(1,2,3), List(1,2,3)) shouldBe List(1,2,3,1,2,3)
    }

    "concatenate 1 list" in {
      concatenate(List(1,2,3)) shouldBe List(1,2,3)
    }

    "concatenate 3 lists" in {
      concatenate(List(1,2,3), List(9, 8, 7), List(11, 20, 1)) shouldBe List(1,2,3,9,8,7,11,20,1)
    }
  }

  "Add one" should {
    "handel Nil" in {
      addOne(Nil) shouldBe Nil
    }

    "handle regular List" in {
      addOne(List(5, 2, 1)) shouldBe List(6, 3, 2)
    }
  }

  "Map" should {
    "handle to String case" in {
      val list = List(1,2,3)
      map(list)(_.toString) shouldBe List("1","2","3")
    }
  }

  "Filter" should {
    "filter out odd numbers" in {
      filter(List(1,2,3,4,5,6,7,8))(_ % 2 == 0) shouldBe List(2,4,6,8)
    }
  }

  "Flatmap" should {
    "work for i => List(i, i)" in {
      val list = List(1,2,3)
      flatMap(list)(i => List(i, i)) shouldBe List(1, 1, 2, 2, 3, 3)
    }
  }

  "Filter 2" should {
    "filter out odd number" in {
      filter2(List(1,2,3,4,5,6,7,8))(_ % 2 == 0) shouldBe List(2,4,6,8)
    }

    "work the same as regular filter" in {
      val l = List(1,2,3,4,5,6,7,8,9)
      val f = (x: Int) => x == 9

      filter2(l)(f) shouldBe filter(l)(f)
    }
  }

  "Add elements" should {
    "correctly add 2 list of the same size" in {
      addElements2(List(1,2,3), List(1,2,3)) shouldBe List(2, 4, 6)
    }

    "converge to shorter list" in {
      addElements2(List(1,2,3,4), List(2,3)) shouldBe List(3, 5)
    }
  }

}
