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
}
