package fpinscala.datastructures

import org.junit.runner.RunWith
import org.scalatest.WordSpec
import org.scalatest.junit.JUnitRunner
import List._

/**
 * Created by pdolega on 12/29/14.
 */
@RunWith(classOf[JUnitRunner])
class ListSpec extends WordSpec {
  "List x value" should {
    "be 3" in {
      assert(x == 3)
    }
  }

  "List tail" should {
    "retrieve everything except head" in {
      assert(tail(List(1, 2, 3, 4)) == List(2, 3, 4))
      assert(tail(List('a', 'b')) == List('b'))
      assert(tail(List("Single element")) == Nil)
    }

    "fail with illegal arguments" in {
      intercept[RuntimeException] {
        tail(List[String]())
      }
      intercept[RuntimeException] {
        tail(Nil)
      }
    }
  }

  "List setHead" should {
    "replace head correctly" in {
      assert(setHead(List(1, 2, 3), 3) == List(3, 2, 3))
      assert(setHead(List('a'), 'z') == List('z'))
    }

    "fail on empty lists" in {
      intercept[RuntimeException] {
        setHead(List[String](), 5)
      }
      intercept[RuntimeException] {
        setHead(Nil, 5)
      }
    }
  }

  "Drop" should {
    "remove first N elements if given number" in {
      assert(drop(List(1, 2, 3, 4, 5), 1) == List(2, 3, 4, 5))
      assert(drop(List(true, false, false), 2) == List(false))
      assert(drop(List(5), 1) == Nil)
    }

    "behave correctly with questionable input" in {
      assert(drop(List(1, 2, 3), 0) == List(1, 2, 3))
      intercept[RuntimeException] {
        drop(List('a', 'b'), 3)
      }
    }

    "remove as long as predicate matches" in {
      assert(dropWhile(List(1, 2, 3), (x: Int) => x < 3) == List(3))
      assert(dropWhile(List(1, 2, 3), (x: Int) => x >5) == List(1, 2, 3))
      assert(dropWhile(List('a', 'b', 'c'), (x: Char) => x < 'z') == Nil)
      assert(dropWhile(Nil, (x: Double) => x == 0.0) == Nil)
    }

    "remove as long as predicate matches and do not continue even if won't match later on" in {
      assert(dropWhile(List(1, 1, 2, 1, 1), (x: Int) => x < 2) == List(2, 1, 1))
    }
  }

  "Init" should {
    "retrieve all except last element" in {
      assert(init(List(1, 2, 3, 4)) == List(1, 2, 3))
      assert(init(List("last")) == Nil)
    }

    "yield error on empty list" in {
      intercept[RuntimeException] {
        init(Nil)
      }
    }
  }

  "length" should {
    "calculate size of the list correctly" in {
      assert(length(List(1, 2, 3)) == 3)
      assert(length(List("last")) == 1)
      assert(length(Nil) == 0)
    }
  }

  "foldLeft" should {
    "retrieve correct results" in {
      assert(foldLeft(List(1, 2, 3), 0)(_+_) == 6)
      assert(foldLeft(List(2, 2, 2), 1)(_*_) == 8)
      assert(foldLeft(List(true, true, true), 0)((acc, _) => acc + 1) == 3)
    }

    "still work with Nil" in {
      assert(foldLeft(Nil: List[Int], 5)(_+_) == 5)
      assert(foldLeft(List[Double](), 5.0)(_*_) == 5.0)
    }
  }

  "functions using foldLeft" should {
    "behave as original for sum" in {
      val ints5 = List(1, 2, 3, 4, 5)
      val ints1 = List(7)
      val ints0 = List[Int]()

      assert(sum(ints5) == sumFoldLeft(ints5))
      assert(sum(ints1) == sumFoldLeft(ints1))
      assert(sum(ints0) == sumFoldLeft(ints0))
    }

    "behave as original for product" in {
      val prod5 = List(1.0, 2.0, 3.0, 4.0, 5.0)
      val prod1 = List(7.0)
      val prod0 = List[Double]()

      assert(product(prod5) == productFoldLeft(prod5))
      assert(product(prod1) == productFoldLeft(prod1))
      assert(product(prod0) == productFoldLeft(prod0))
    }

    "behave as original for length" in {
      val char5 = List('a', 'b', 'c', 'd', 'e')
      val double1 = List(7.0)
      val empty = Nil

      assert(length(char5) == lengthFoldLeft(char5))
      assert(length(double1) == lengthFoldLeft(double1))
      assert(length(empty) == lengthFoldLeft(empty))
    }
  }
}
