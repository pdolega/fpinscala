package fpinscala.laziness

import fpinscala.laziness.Stream._
import org.junit.runner.RunWith
import org.scalatest.WordSpec
import org.scalatest.junit.JUnitRunner

/**
 * Tests for either class.
 */
@RunWith(classOf[JUnitRunner])
class StreamSpec extends WordSpec {

  "Stream trait methods" should {
    "have correct implementation of toList" in {
      assert(Stream(1, 2, 3, 4, 5).toList == List(1, 2, 3, 4, 5))
      assert(Cons(() => 'a', () => empty).toList == List('a'))
      assert(Stream().toList == Nil)
    }

    "have correct implementation of take & drop" in {
      assert(Stream(1, 2, 3, 4, 5).take(1).toList == Stream(1).toList)
      assert(Stream(1, 2, 3).take(3).toList == Stream(1, 2, 3).toList)
      assert(empty.take(0) == empty)

      assert(Stream(1, 2, 3, 4, 5).drop(1).toList == List(2, 3, 4, 5))
      assert(Stream(4, 5).drop(2) == empty)
      assert(Stream(1, 2).drop(0).toList == Stream(1, 2).toList)
      assert(empty.drop(0) == empty)
    }

    "have correct implementation of takeWhile" in {
      assert(Stream('a', 'b', 'c', 'f').takeWhile(_ < 'c').toList == List('a', 'b'))
      assert(Stream('a', 'b', 'c', 'f').takeWhile(_ > 'z').toList == Nil)
      assert(Empty.takeWhile(_ => true).toList == Nil)
    }
  }
}
