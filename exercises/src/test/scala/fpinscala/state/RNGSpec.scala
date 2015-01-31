package fpinscala.state

import fpinscala.state.RNG.Simple
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
      val entries = unfoldRandom(rng, RNG.nonNegativeInt)
      assert(entries.take(100).forall(_ >= 0))
    }

    "have correct implementation of nonNegativeInt with MinInt" in {
      val fabricatedRng = new RNG {
        override def nextInt: (Int, RNG) = if(System.currentTimeMillis() % 2 == 0) (Integer.MIN_VALUE, this) else (0, this)
      }
      val entriesMinInt = unfoldRandom(fabricatedRng, RNG.nonNegativeInt)
      assert(entriesMinInt.take(100).forall { _ >= 0 })
    }

    "have correct implementation of double returning <0.0, 1.0)" in {
      val entries = unfoldRandom(rng, RNG.double)
      assert(entries.take(100).forall { d => checkDoubleRange(d) })
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
  }

  private def unfoldRandom[A](seed: RNG, randFunc: RNG => (A, RNG)) = {
    unfold(seed) { rand =>
      val (a, newRand) = randFunc(rand)
      Option((a, newRand))
    }
  }

  private def checkDoubleRange(d: Double) = {
    d >= 0.0 && d < 1.0
  }
}