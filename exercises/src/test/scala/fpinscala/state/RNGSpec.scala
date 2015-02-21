package fpinscala.state

import fpinscala.state.RNG.{nonNegativeInt, Simple}
import org.junit.runner.RunWith
import org.scalatest.WordSpec
import org.scalatest.junit.JUnitRunner
import scalaz._
import Scalaz._

/**
 * Tests for RNG class.
 */
@RunWith(classOf[JUnitRunner])
class RNGSpec extends WordSpec {

  val rng = new Simple(System.currentTimeMillis())

  "RNG trait methods" should {
    "have correct implementation of nonNegativeInt" in {
      val entries = unfoldRandom(rng, nonNegativeInt)
      assert(entries.take(100).forall(_ >= 0))
    }

    "have correct implementation of nonNegativeInt with MinInt" in {
      val fabricatedRng = new RNG {
        override def nextInt: (Int, RNG) = if(System.currentTimeMillis() % 2 == 0) (Integer.MIN_VALUE, this) else (0, this)
      }
      val entriesMinInt = unfoldRandom(fabricatedRng, nonNegativeInt)
      assert(entriesMinInt.take(100).forall { _ >= 0 })
    }

    "have correct implementation of double and doubleViaMap returning <0.0, 1.0)" in {
      assert(unfoldRandom(rng, RNG.double).take(100).forall { checkDoubleRange })
      assert(unfoldRandom(rng, RNG.doubleViaMap).take(100).forall { checkDoubleRange })
    }

    "have correct intDouble, doubleInt, double3 functions" in {
      val intDoubleEntries = unfoldRandom(rng, RNG.intDouble)
      assert(intDoubleEntries.take(100).forall { case (i, d) =>
        i >= Int.MinValue && i <= Int.MaxValue &&
        checkDoubleRange(d)
      })

      val doubleIntEntries = unfoldRandom(rng, RNG.doubleInt)
      assert(doubleIntEntries.take(100).forall { case (d, i) =>
        checkDoubleRange(d) &&
        i >= Int.MinValue && i <= Int.MaxValue
      })

      val double3Entries = unfoldRandom(rng, RNG.double3)
      assert(double3Entries.take(100).forall { case (d1, d2, d3) =>
          checkDoubleRange(d1) && checkDoubleRange(d2) && checkDoubleRange(d3)
      })
    }

    "have correct implementation of ints" in {
      val (ints1, _) = RNG.ints(500)(rng)
      assert(ints1.size == 500)
      assert(ints1.forall { checkIntRange })

      assert(RNG.intsViaSequence(500)(rng)._1.forall { checkIntRange })
    }

    "have correct implementation of map2" in {
      val rng1 = new RNG { override def nextInt: (Int, RNG) = (1, this) }
      assert(RNG.map2(nonNegativeInt, nonNegativeInt) { _ + _ }(rng1)._1 == 2)

      val rng2 = new RNG { override def nextInt: (Int, RNG) = (3, this) }
      assert(RNG.map2(nonNegativeInt, RNG.double) { _ - _ }(rng2)._1 == 3 - (3.0 / Int.MaxValue))

      assert(RNG.map2(RNG.doubleInt, RNG.double) { case ((d1, i), d2) => d1 * d2 + i }(rng2)._1 ==
        (3.0 / Int.MaxValue) * (3.0 / Int.MaxValue) + 3.0)
    }

    "have correct implementation of sequence" in {
      assert(RNG.sequence(List.fill(50) { RNG.nonNegativeInt _ })(rng)._1.forall { i => i >= Int.MinValue && i <= Int.MaxValue })
      assert(RNG.sequence(List.fill(50) { RNG.double _ })(rng)._1.forall { checkDoubleRange })
      assert(RNG.sequence(List(RNG.unit(1), RNG.unit(2), RNG.unit(3)))(rng)._1 == List(1, 2, 3))
    }

    "have correct flatmap implementation" in {
      assert(RNG.flatMap(RNG.unit(5)) { a => RNG.unit(a + 1) }(rng)._1 == 6)

      (0 until 100).foreach { _ =>
        val doubleRand = RNG.flatMap(RNG.doubleViaMap) { a => RNG.unit(a + 1)}(rng)._1
        assert(doubleRand >= 1 && doubleRand < 2)
      }
    }

    "have correct implementation of nonNegativeLessThan" in {
      (1000 until 2000).foreach { i =>
        val (value, _) = RNG.nonNegativeLessThan(i)(rng)
        assert(value < i && value >= 0)
      }
    }
  }

  private def unfoldRandom[A](seed: RNG, randFunc: RNG => (A, RNG)) = {
    unfold(seed) { rand =>
      val (a, newRand) = randFunc(rand)
      Option((a, newRand))
    }
  }

  private def checkIntRange(i: Int) = i >= Int.MinValue && i < Int.MaxValue

  private def checkDoubleRange(d: Double) = d >= 0.0 && d < 1.0
}