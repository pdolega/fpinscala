package fpinscala.state

import fpinscala.state.RNG.Simple
import org.junit.runner.RunWith
import org.scalatest.WordSpec
import org.scalatest.junit.JUnitRunner

/**
 * Tests for State class.
 */
@RunWith(classOf[JUnitRunner])
class StateSpec extends WordSpec {

  val rng = new Simple(System.currentTimeMillis())

  "State" should {
    "have correct implementation of map2" in {
      val int1 = State.unit[RNG, Int](1)
      val int2 = State.unit[RNG, Int](2)

      assert(int1.map2(int2) { (a, b) => a.toDouble + b.toDouble + 1 }.run(rng)._1 == 4.0)
    }

  }
}