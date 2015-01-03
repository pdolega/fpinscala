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

  final val ExtensiveTree = Branch(
    Leaf(1), Branch(
      Branch(
        Leaf(1), Branch(
          Leaf(17), Leaf(15))),
      Leaf(2)))
  
  "Tree" should {
    "have count working properly on simple trees" in {
      assert(count(Leaf(1)) == 1)
      assert(count(Branch(Leaf(1), Leaf(2))) == 2)
      assert(count(Branch(Branch(Leaf(1), Branch(Leaf(2), Leaf(3))), Leaf(4))) == 4)
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

      assert(depth(ExtensiveTree) == 5)
    }

    "have map working correctly" in {
      assert(map(Leaf(5)){ _.toString } == Leaf("5"))

      assert(map(ExtensiveTree) { _.toDouble + 0.5 } ==
        Branch(
          Leaf(1.5), Branch(
            Branch(
              Leaf(1.5), Branch(
                Leaf(17.5), Leaf(15.5))),
            Leaf(2.5)))
      )
    }
  }

  "all above reimplemented with fold" should {
    "work for count" in {
      assert(fold(Leaf(5)) { x => 1 } { _ + _ } == 1 )
      assert(fold(ExtensiveTree) { x => 1 } { _ + _ } == 5 )
    }

    "work for max" in {
      assert(fold(Leaf(5)) { x => x } { _ max _ } == 5 )
      assert(fold(ExtensiveTree) { x => x } { _ max _ } == 17 )
    }

    "work for depth" in {
      assert(fold(Leaf(5)) { x => 1 } { (x, y) => (x + 1) max (y + 1) } == 1 )
      assert(fold(ExtensiveTree) { x => 1 } { (x, y) => (x + 1) max (y + 1) } == 5 )
    }

    "work for map" in {
      assert(
        fold(Leaf(5)) { x => Leaf(x.toString) : Tree[String] } { Branch(_, _) } == Leaf("5")
      )

      assert(
        fold(ExtensiveTree) { x => Leaf(x.toDouble + 0.5): Tree[Double] } { Branch(_, _) }
          ==
          Branch(
            Leaf(1.5), Branch(
              Branch(
                Leaf(1.5), Branch(
                  Leaf(17.5), Leaf(15.5))),
              Leaf(2.5)))
      )
    }
  }
}
