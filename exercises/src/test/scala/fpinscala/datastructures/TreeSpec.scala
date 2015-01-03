package fpinscala.datastructures

import fpinscala.datastructures.Tree._
import org.junit.runner.RunWith
import org.scalatest.WordSpec
import org.scalatest.junit.JUnitRunner

/**
 * Tests for tree class.
 */
@RunWith(classOf[JUnitRunner])
class TreeSpec extends WordSpec {
  "Tree" should {
    "have size working properly on simple trees" in {
      assert(size(Leaf(1)) == 1)
      assert(size(Branch(Leaf(1), Leaf(2))) == 2)
      assert(size(Branch(Branch(Leaf(1), Branch(Leaf(2), Leaf(3))), Leaf(4))) == 4)
    }

    "have max work as expected" in {
      assert(max(Leaf(5)) == 5)
      assert(max(Branch(Leaf(5), Leaf(5))) == 5)
      assert(max(Branch(Branch(Leaf(1), Branch(Leaf(7), Leaf(2))), Leaf(3))) == 7)
    }

    "have depth working as expected" in {
      assert(depth(Leaf(5)) == 1)

      assert(depth(Branch(
        Leaf(1), Branch(
          Leaf(1), Leaf(2)
        ))) == 3)

      assert(depth(Branch(
        Leaf(1), Branch(
          Branch(
            Leaf(1), Branch(
              Leaf(17), Leaf(15))),
          Leaf(2)))
      ) == 5)
    }
  }
}
