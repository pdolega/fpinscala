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

      assert(Stream(1, 2, 3, 4, 5).takeUnfold(1).toList == Stream(1).toList)
      assert(Stream(1, 2, 3).takeUnfold(3).toList == Stream(1, 2, 3).toList)
      assert(empty.takeUnfold(0) == empty)

      assert(Stream(1, 2, 3, 4, 5).drop(1).toList == List(2, 3, 4, 5))
      assert(Stream(4, 5).drop(2) == empty)
      assert(Stream(1, 2).drop(0).toList == Stream(1, 2).toList)
      assert(empty.drop(0) == empty)
    }

    "have correct implementation of takeWhile" in {
      assert(Stream(1, 2, 3, 4, 5).takeWhile(_ < 2).toList == List(1))
      assert(Stream('a', 'b', 'c', 'f').takeWhile(_ < 'c').toList == List('a', 'b'))
      assert(Stream('a', 'b', 'c', 'f').takeWhile(_ => true).toList == List('a', 'b', 'c', 'f'))
      assert(Stream('a', 'b', 'c', 'f').takeWhile(_ > 'z').toList == Nil)
      assert(Empty.takeWhile(_ => true).toList == Nil)

      assert(Stream(1, 2, 3, 4, 5).takeWhileFoldRight(_ < 2).toList == List(1))
      assert(Stream('a', 'b', 'c', 'f').takeWhileFoldRight(_ < 'c').toList == List('a', 'b'))
      assert(Stream('a', 'b', 'c', 'f').takeWhileFoldRight(_ => true).toList == List('a', 'b', 'c', 'f'))
      assert(Stream('a', 'b', 'c', 'f').takeWhileFoldRight(_ > 'z').toList == Nil)
      assert(Empty.takeWhileFoldRight(_ => true).toList == Nil)

      assert(Stream(1, 2, 3, 4, 5).takeWhileUnfold(_ < 2).toList == List(1))
      assert(Stream('a', 'b', 'c', 'f').takeWhileUnfold(_ < 'c').toList == List('a', 'b'))
      assert(Stream('a', 'b', 'c', 'f').takeWhileUnfold(_ => true).toList == List('a', 'b', 'c', 'f'))
      assert(Stream('a', 'b', 'c', 'f').takeWhileUnfold(_ > 'z').toList == Nil)
      assert(Empty.takeWhileUnfold(_ => true).toList == Nil)
    }

    "have correct implementation of forAll" in {
      assert(!Stream(1, 2, 3, 4, 5).forAll(_ > 10))
      assert(!Stream('a', 'b', 'c', 'f').forAll(_ < 'c'))
      assert(Stream('a', 'b', 'c', 'f').forAll(_ < 'z'))
      assert(Stream[Int]().forAll(x => false))
    }

    "have correct headOption" in {
      assert(Stream(1, 2, 3, 4).headOption == Some(1))
      assert(Stream(1).headOption == Some(1))
      assert(Stream[Int]().headOption == None)
    }

    "have correct map & flatmap implementation" in {
      assert(Stream(1,2,3).map{ _.toString }.toList == Stream("1", "2", "3").toList)
      assert(Stream[Int]().map{ _.toString }.toList == Stream[String]().toList)

      assert(Stream(1,2,3).flatMap{ x => Stream(x.toString + 1, x.toString + 2, x.toString + 3) }.toList == List("11", "12", "13", "21", "22", "23", "31", "32", "33"))
      assert(Stream[Int]().flatMap{ x => Stream[String]() }.toList == List[String]())

      assert(Stream(1,2,3).mapUnfold{ _.toString }.toList == Stream("1", "2", "3").toList)
      assert(Stream[Int]().mapUnfold{ _.toString }.toList == Stream[String]().toList)
    }

    "have correct append" in {
      assert(Stream(1,2,3).append(Stream(4, 5)).toList == List(1, 2, 3, 4, 5))
      assert(Stream[Int]().append(Stream(1, 2)).toList == List(1, 2))
      assert(Stream[Int]().append(Empty).toList == Nil)
    }

    "have correct filter" in {
      assert(Stream(1,2,3, 4).filter(_ < 3).toList == List(1, 2))
      assert(Stream[Int]().filter(_ < 5).toList == Nil)
      assert(Stream[Int](1, 2, 3).filter( _ > 15).toList == Nil)
    }

    "have correct implementation of constant" in {
      assert(Stream.constant("1").take(3).toList == List("1", "1", "1"))
      assert(Stream.constant(1).take(3).toList == List(1, 1, 1))

      assert(Stream.constantUnfold(1).take(3).toList == List(1, 1, 1))
      assert(Stream.constantUnfold(1).take(3).toList == List(1, 1, 1))
    }

    "have correct implementation of from" in {
      assert(Stream.from(0).take(3).toList == List(0, 1, 2))
      assert(Stream.from(-5).take(3).toList == List(-5, -4, -3))

      assert(Stream.fromUnfold(-5).take(3).toList == List(-5, -4, -3))
      assert(Stream.fromUnfold(-5).take(3).toList == List(-5, -4, -3))
    }

    "have correct implementation of fibs" in {
      assert(Stream.fibs.take(5).toList == List(1, 1, 2, 3, 5))
      assert(Stream.fibs.drop(10).take(3).toList == List(89, 144, 233))

      assert(Stream.fibsUnfold.drop(10).take(3).toList == List(89, 144, 233))
      assert(Stream.fibsUnfold.drop(10).take(3).toList == List(89, 144, 233))
    }

    "have correct implementation of unfold" in {
      assert(Stream.unfold((0, 1)) { state => Some((state._1, (state._2, state._1 + state._2)))}.take(7).toList == List(0, 1, 1, 2, 3, 5, 8))
      assert(Stream.unfold((1)) { state => Some((state, (state + 1)))}.take(5).toList == List(1, 2, 3, 4, 5))
      assert(Stream.unfold((1)) { state => if(state <= 10) Some((state, (state + 1))) else None}.toList == List(1, 2, 3, 4, 5, 6, 7, 8, 9, 10))
    }

    "have correct implementation of zipWith and zipAll" in {
      assert(Stream(1, 2, 3).zipWith(Stream(1, 2, 3)).toList == List((1,1),(2,2),(3,3)))
      assert(Stream(1, 2, 3).zipWith(Stream(1)).toList == List((1,1)))
      assert(Stream(1, 2, 3).zipWith(Stream("1", "2", "3", "4")).toList == List((1,"1"),(2,"2"),(3,"3")))


      assert(Stream(1, 2).zipAll(Stream("1", "2", "3")).toList == List((Some(1),Some("1")),(Some(2),Some("2")),(None,Some("3"))))
      assert(Stream(1, 2, 3).zipAll(Stream("1", "2")).toList == List((Some(1),Some("1")),(Some(2),Some("2")),(Some(3),None)))
    }
  }
}