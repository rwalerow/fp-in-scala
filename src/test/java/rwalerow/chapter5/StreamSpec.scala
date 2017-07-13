package rwalerow.chapter5

import org.scalatest.{Matchers, WordSpec}
import rwalerow.chapter5.Stream.{constant, fibs, unfold}

class StreamSpec extends WordSpec with Matchers {

  "Stream" should {

    val streamWithExceptionAt2nd = Cons(() => 2,
      () => Cons(() => { throw new Exception("sad"); 2},
        () => Cons(() => 2,
          () => Cons(() => 2,
            () => Cons(() => 2,
              () => Empty)))))

    "toList" should {
      "work for empty stream" in {
        val a = Stream(1, 2, 3, 4, 5, 6)
        a.toList shouldBe List(1, 2, 3, 4, 5, 6)
      }
    }
    "take" should {
      "take only requerd number of elements" in {
        val a = Stream(1, 2, 3, 4, 5)
        val taken = a.take(3).toList
        val viaFold = a.takeViaFold(3).toList
        taken.size shouldBe 3
        taken.last shouldBe 3
        viaFold.size shouldBe 3
        viaFold.last shouldBe 3
      }
      "not call untaken elements" in {
        streamWithExceptionAt2nd.take(1).toList shouldBe List(2)
        streamWithExceptionAt2nd.takeViaFold(1).toList shouldBe List(2)
      }
    }

    "takeWhile" should {
      "take only even elements" in {
        val a = Stream(2,4,6,8,11,12)
        a.takeWhile(_ % 2 == 0).toList shouldBe List(2,4,6,8)
        a.takeWhileViaFold(_ % 2 == 0).toList shouldBe List(2,4,6,8)
      }
    }

    "for all" should {
      "work for all" in {
        Stream(2,4,5,6,7).forAll(_ > 0) should be (true)
      }
      "detect false case" in {
        Stream(2,3,4,5,6).forAll(_ != 6) should be (false)
      }
    }

    "head option" should {
      "return some with element" in {
        Stream(1,2).headOption shouldBe Some(1)
      }
      "return emnty" in {
        Empty.headOption shouldBe None
      }
    }

    "map" should {
      "transform stream into string" in {
        (Stream(1,2,3,4,5) map (_.toString) toList) shouldBe List("1", "2", "3", "4", "5")
        (Stream(1,2,3,4,5) mapViaFold (_.toString) toList) shouldBe List("1", "2", "3", "4", "5")
      }
      "call should be lazy" in {
        streamWithExceptionAt2nd map (_.toString)
        streamWithExceptionAt2nd mapViaFold (_.toString)
      }
    }

    "filter" should {
      "filter out all odd elements" in {
        Stream(1, 2, 3, 4, 5, 6).filter(_ % 2 == 0).toList shouldBe List(2, 4, 6)
      }
    }

    "flatMap" should {
      "transform tranform a stream" in {
        Stream(1, 2, 3).flatMap(x => Stream(x, x, x)).toList shouldBe List(1,1,1,2,2,2,3,3,3)
      }
      "transform with type change" in {
        Stream(1,2,3,4).flatMap(x => Stream(Array.fill(x)(x.toString): _*)).toList shouldBe List("1", "2", "2", "3", "3", "3", "4", "4", "4", "4")
      }
    }

    "append" should {
      "Add 2 streams" in {
        Stream(1,2,3).append(Stream(4,5,6)).toList shouldBe List(1,2,3,4,5,6)
      }
    }

    "constants" should {
      "return infinite 2 list" in {
        constant(2).take(10).toList.sum shouldBe 20
      }
    }

    "from" should {
      "generate 5 element" in {
        Stream.from(1).take(5).toList shouldBe List(1,2,3,4,5)
      }

      "sum 10 first elements" in {
        Stream.from(1).take(10).toList.sum shouldBe 55
      }
    }

    "fibs" should {
      "generate proper first 5 elements" in {
        fibs.take(5).toList shouldBe List(0, 1, 1, 2, 3)
      }

      "generate proper first 10 elements" in {
        fibs.take(10).toList shouldBe List(0, 1, 1, 2, 3, 5, 8, 13, 21, 34)
      }
    }

    "unfold" should {
      "generate const stream of elements" in {
        val result = unfold(1)(_ => Some(1, 1)).take(10).toList

        result.size shouldBe 10
        result.sum shouldBe 10
        result.product shouldBe 1
      }

      "generate increasing stream" in {
        val result = unfold(1)(x => Some(x, x + 10))
        result.take(5).toList shouldBe List(1, 11, 21, 31, 41)
      }

      "terminate stream on None value" in {
        val result = unfold(1)(x => if(x < 3) Some(x, x + 1) else None)
        result.take(10).toList shouldBe List(1,2)
      }
    }

    "zipWith via fold" should {
      "zip Empty with some" in {
        val first = Empty
        val second = constant(10)

        (first zipWithViaFold second).take(10).toList shouldBe Nil
      }
      "zip some with empty" in {
        val first = constant(10)
        val second = Empty

        (first zipWithViaFold second).take(10).toList shouldBe Nil
      }
      "zip 2 inifinite Streams" in {
        val count = Stream.from(1)
        val fibsonacci = fibs

        (count zipWithViaFold fibsonacci).take(5).toList shouldBe List(
          (1, 0), (2, 1), (3, 1), (4, 2), (5, 3)
        )
      }
    }

    "zipAll" should {
      "zip Empty whith other" in {
        val f = Empty
        val s = Stream(1, 2)

        f.zipAll(s).toList shouldBe List(
          (None, Some(1)), (None, Some(2))
        )

        s.zipAll(f).toList shouldBe List(
          (Some(1), None), (Some(2), None)
        )
      }

      "work the same as normal zip for regular zip" in {
        val f = Stream(1,2,3,4,5,6)
        val s = Stream(6,5,4,3,2,1)

        val expected = f.zipWithViaFold(s).map{ case(x, y) => (Some(x), Some(y)) }

        f.zipAll(s).toList shouldBe expected.toList
      }
    }

    "startWith" should {
      "return false on longer start string" in {
        Stream(1,2,3,4).startWith(Stream(1,2,3,4,5,6,7)) shouldBe false
      }

      "return false on invalid substring" in {
        Stream(1,2,3,4,5,6,7).startWith(Stream(1,2,3,4,3)) shouldBe false
      }

      "detect proper substring" in {
        Stream.from(1).take(99).startWith(Stream.from(1).take(98)) shouldBe true
      }

    }

    "tails" should {
      "Find all tails of Stream(1,2,3,4)" in {
        Stream(1,2,3,4).tails.map(_.toList).toList shouldBe List(
          List(1,2,3,4), List(2,3,4), List(3,4), List(4)
        )
      }

      "Find all tailsViaScanRight of Stream(1,2,3,4)" in {
        Stream(1,2,3,4).tailsViaScanRight.map(_.toList).toList shouldBe List(
          List(1,2,3,4), List(2,3,4), List(3,4), List(4)
        )
      }
    }

  }

}
