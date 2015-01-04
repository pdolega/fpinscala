package fpinscala.errorhandling

import org.junit.runner.RunWith
import org.scalatest.WordSpec
import org.scalatest.junit.JUnitRunner
import Option._

/**
 * Tests for option class.
 */
@RunWith(classOf[JUnitRunner])
class OptionSpec extends WordSpec {

  "Option trait methods" should {
    "have map working perfectly" in {
      assert(Some("1").map { _.toInt } == Some(1))
      assert((None: Option[String]).map { _.toInt } == None)
    }

    "have getOrElse working fine" in {
      assert(Some(1).getOrElse(2) == 1)
      assert(None.getOrElse(2) == 2)

      class Fruit
      class Apple extends Fruit
      assert(Some(new Fruit).getOrElse(new Apple).isInstanceOf[Fruit])
      assert(None.getOrElse(new Apple).isInstanceOf[Apple])
    }

    "have flatMap working nice !" in {
      assert(Some(1).flatMap { x => Some(x.toString) } == Some("1"))
      assert(None.flatMap { x => Some(x.toString) } == None)
      assert(Some(1).flatMap { x => None: Option[String] }.flatMap { x => Some(x.toInt) } == None)
    }

    "have orElse working good" in {
      assert(Some(1).orElse(None) == Some(1))
      assert(None.orElse(None) == None)
      assert(None.orElse(Some(5)) == Some(5))
    }

    "have filter working fine" in {
      assert(Some(1).filter(_ > 5) == None)
      assert(Some(1).filter(_ < 5) == Some(1))
    }
  }

  "Option companion methods" should {
    "have variance working fine" in {
      assert(variance(Seq(1.0, 2.0, 3.0)) == Some(2.0 / 3))
      assert(variance(Seq(1.0, 2.0, 3.0, 4.0, 5.0)) == Some(10.0 / 5))
    }

    "have map2 working cool" in {
      assert(map2(Some(1), Some(2)) { _ + _ } == Some(3))
      assert(map2(None: Option[Int], Some(2)) { _ + _ } == None)
      assert(map2(Some(1), None) { _ + _ } == None)

      assert(map2ViaFor(Some(1), Some(2)) { _ + _ } == Some(3))
      assert(map2ViaFor(None: Option[Int], Some(2)) { _ + _ } == None)
      assert(map2ViaFor(Some(1), None) { _ + _ } == None)
    }

    "have sequence working good" in {

      def runAsserts(f: List[Option[Int]] => Option[List[Int]]) {
        assert(f(List(Some(1), Some(2), Some(3))) == Some(List(1, 2, 3)))
        assert(f(List(Some(1), Some(2), None)) == None)
        assert(f(List(Some(1), None, Some(3))) == None)
        assert(f(List(None, Some(2), Some(3))) == None)
        assert(f(List(None)) == None)
        assert(f(List()) == Some(List()))
      }

      runAsserts(sequence)
      runAsserts(sequenceViaFor)
      runAsserts(sequenceViaTraverse)
    }

    "have traverse working fine" in {
      assert(traverse(List(1, 2, 3)) { x => Some(x.toString) } == Some(List("1", "2", "3")))
      assert(traverse(List(1, 2, 3)) { x => if( x > 2) None else Some(x.toString) } == None)
      assert(traverse(Nil) { x => Some(x) } == Some(Nil))
    }
  }
}
