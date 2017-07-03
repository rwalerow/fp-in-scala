package rwalerow.chapter5

import org.scalatest.{Matchers, WordSpec}

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
        taken.size shouldBe 3
        taken.last shouldBe 3
      }
      "not call untaken elements" in {
        streamWithExceptionAt2nd.take(1).toList shouldBe List(2)
      }
    }

    "takeWhile" should {
      "take only even elements" in {
        val a = Stream(2,4,6,8,11,12)
        a.takeWhile(_ % 2 == 0).toList shouldBe List(2,4,6,8)
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
      }
      "call should be lazy" in {
        streamWithExceptionAt2nd map (_.toString)
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
        Stream.constant(2).take(10).toList.sum shouldBe 20
      }
    }
  }

}
