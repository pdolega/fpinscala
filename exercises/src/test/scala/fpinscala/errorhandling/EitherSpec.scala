package fpinscala.errorhandling

import fpinscala.errorhandling.Either._
import org.junit.runner.RunWith
import org.scalatest.WordSpec
import org.scalatest.junit.JUnitRunner

/**
 * Tests for either class.
 */
@RunWith(classOf[JUnitRunner])
class EitherSpec extends WordSpec {

  "Either trait methods" should {
    "have map working fine" in {
      assert(Right(1).map( _.toString ) == Right("1"))
      assert(Right(1).map( _ + 1 ).map( _ + 1 ) == Right(3))
      assert(Left(-1).map( _.toString ) == Left(-1))
    }

    "have flatMap working fine" in {
      assert(
        Right(1).flatMap { x => Right(x + 1) }.flatMap { x => Right(x + 1) } == Right(3))
      assert(
        Right(1).flatMap { x => Right(x + 1) }.flatMap { x => Left(-1) } == Left(-1))
      assert(
        Right(1).flatMap { x => Right(x + 1) }.flatMap { x => Left(-1): Either[Int, Int] }.flatMap{ x => Right(x + 1) } == Left(-1))
      assert(
        (Left(-1) : Either[Int, Int]).flatMap { x => Right(x + 1) } == Left(-1))
    }

    "have orElse working fine" in {
      assert(Left(-1).orElse(Right(1)) == Right(1))
      assert(Right(1).orElse(Left(-1)) == Right(1))
      assert(Right(1).orElse(Right(2)) == Right(1))
      assert(Left(-1).orElse(Left(-2)) == Left(-2))
    }

    "have map2 working fine" in {
      assert(Right(1).map2(Right(2)) { _ + _ } == Right(3))
      assert((Left(-1): Either[Int, Int]).map2(Right(2)) { _ + _ } == Left(-1))
      assert(Right(1).map2(Left(-1)) { _ + _ } == Left(-1))
      assert((Left(-1): Either[Int, Int]).map2(Left(-2): Either[Int, Int]) { _ + _ } == Left(-1))
    }
  }
}
