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
}
