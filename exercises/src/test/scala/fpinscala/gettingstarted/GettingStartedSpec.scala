package fpinscala.gettingstarted

import org.junit.runner.RunWith
import org.scalatest.WordSpec
import org.scalatest.junit.JUnitRunner
import fpinscala.gettingstarted.MyModule._
import fpinscala.gettingstarted.PolymorphicFunctions._

/**
 * Created by pdolega on 12/28/14.
 */
@RunWith(classOf[JUnitRunner])
class GettingStartedSpec extends WordSpec {
  "Fibonacci" should {
    "Retrieve correct results for simple cases" in {
      assert(fib(1) == 1)
      assert(fib(2) == 1)
      assert(fib(3) == 2)
      assert(fib(4) == 3)
      assert(fib(5) == 5)
    }

    "Relatively far fibbonacci numbers should be correct" in {
      assert(fib(15) == 610)
    }

    "Incorrect fibbonacci sequence elements should yield error" in {
      intercept[IllegalArgumentException] {
        fib(-5)
      }
    }
  }

  "isSorted" should {
    "work correctly for unordered arrays of Ints" in {
      val gtInt = (x: Int, y: Int) => x > y
      assert(!isSorted(Array(5,3,2), gtInt))
      assert(!isSorted(Array(7,2,9), gtInt))
      assert(!isSorted(Array(2,1,1), gtInt))
    }

    "work correctly for arrays of Strings" in {
      val gtString = (x: String, y: String) => x > y
      assert(!isSorted(Array("Keyser Soze","Jim Jarmush", "Tommy Lee Jones"), gtString))
      assert(isSorted(Array("a","b","c"), gtString))
      assert(isSorted(Array("Haskell", "Java" , "Ruby", "Scala"), gtString))
    }

    "work correctly for corner cases" in {
      assert(isSorted(Array[String](), (x: String, y: String) => x > y))
      assert(isSorted(Array(5.0), (x: Double, y: Double) => x > y))
    }
  }

  "currying" should {
    "work correctly for general cases" in {
      val curriedAdd = curry((x: Int, y: Int) => x + y)
      assert(curriedAdd(1)(2) == 3)
      assert(curriedAdd(10)(15) == 25)
    }

    "work correctly for case with different types" in {
      val curriedInterpolation = curry((x: Int, y: Double) => s"${x + y}")
      assert(curriedInterpolation(1)(0.5) == "1.5")
    }
  }

  "uncurrying" should {
    "work correctly for general cases" in {
      val curriedAdd = curry((x: Int, y: Int) => x + y)
      assert(uncurry(curriedAdd)(1, 2) == 3)
      assert(uncurry(curriedAdd)(10, 15) == 25)
    }

    "work correctly for case with different types" in {
      val curriedInterpolation = curry((x: Int, y: Double) => s"${x + y}")
      assert(uncurry(curriedInterpolation)(1, 0.5) == "1.5")
    }
  }

  "compose" should {
    "work correctly for general cases" in {
      val interpolate = (x: Int) => x.toString
      val string2DoublePlusOneTenth = (string: String) => java.lang.Double.parseDouble(string) + 0.1

      assert(compose(string2DoublePlusOneTenth, interpolate)(5) == 5.1)
    }
  }
}
