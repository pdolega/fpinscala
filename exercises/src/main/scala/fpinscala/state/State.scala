package fpinscala.state

import scala.annotation.tailrec

trait RNG {
  def nextInt: (Int, RNG) // Should generate a random `Int`. We'll later define other functions in terms of `nextInt`.
}

object RNG {
  // NB - this was called SimpleRNG in the book text

  case class Simple(seed: Long) extends RNG {
    def nextInt: (Int, RNG) = {
      val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL // `&` is bitwise AND. We use the current seed to generate a new seed.
      val nextRNG = Simple(newSeed) // The next state, which is an `RNG` instance created from the new seed.
      val n = (newSeed >>> 16).toInt // `>>>` is right binary shift with zero fill. The value `n` is our new pseudo-random integer.
      (n, nextRNG) // The return value is a tuple containing both a pseudo-random integer and the next `RNG` state.
    }
  }

  type Rand[+A] = RNG => (A, RNG)

  val int: Rand[Int] = _.nextInt

  def unit[A](a: A): Rand[A] =
    rng => (a, rng)

  def map[A,B](s: Rand[A])(f: A => B): Rand[B] = flatMap(s) { a => unit(f(a)) }

  def nextInt(rng: RNG): (Int, RNG) = rng.nextInt

  @tailrec
  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    val (number, newRng) = rng.nextInt
    if(number >= 0) (Math.abs(number), newRng) else nonNegativeInt(newRng)
  }

  def double(rng: RNG): (Double, RNG) = {
    val (intNum, newRng) = nonNegativeInt(rng)
    val doubleNum = (intNum % Int.MaxValue / Int.MaxValue.toDouble)
    (doubleNum, newRng)
  }

  def doubleViaMap: Rand[Double] = {
    map(nonNegativeInt) { _ / (Int.MaxValue.toDouble + 1.0) }
  }

  def intDouble(rng: RNG): ((Int,Double), RNG) = {
    val (i, newRng) = rng.nextInt
    val (d, nextRng) = double(newRng)
    ((i, d), nextRng)
  }

  def doubleInt(rng: RNG): ((Double,Int), RNG) = {
    val ((i, d), newRng) = intDouble(rng)
    ((d, i), newRng)
  }

  def double3(rng: RNG): ((Double,Double,Double), RNG) = {
    val (d1, nextRng1) = double(rng)
    val (d2, nextRng2) = double(nextRng1)
    val (d3, nextRng3) = double(nextRng2)
    ((d1, d2, d3), nextRng3)
  }

  def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
    @tailrec
    def generate(num: Int, newRng: RNG, generated: List[Int]): (List[Int], RNG) = {
      if(num > 0) {
        val (i, nextRng) = newRng.nextInt
        generate(num - 1, nextRng, i :: generated)
      } else {
        (generated, newRng)
      }
    }

    generate(count, rng, Nil)
  }

  def intsViaSequence(count: Int): Rand[List[Int]] = sequence(List.fill(count)(nextInt))

  def map2[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = flatMap(ra) { a =>
    map(rb) { b =>
      f(a, b)
    }
  }

  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] = {
    rng => {
      val (results, newRng) = fs.foldLeft((Nil: List[A], rng)) { case ((results, currRng), elem) =>
        val (a, newRng) = elem(rng)
        (a :: results, newRng)
      }
      (results.reverse, newRng)
    }
  }

  def flatMap[A,B](f: Rand[A])(g: A => Rand[B]): Rand[B] = { rng: RNG =>
    val (a, newRng) = f(rng)
    g(a)(newRng)
  }

  def nonNegativeLessThan(n: Int): Rand[Int] = flatMap(nonNegativeInt) { nonNegative => RNG.unit(nonNegative % n) }
}

case class State[S,+A](run: S => (A, S)) {
  def map[B](f: A => B): State[S, B] = flatMap { a => State.unit(f(a)) }

  def map2[B,C](sb: State[S, B])(f: (A, B) => C): State[S, C] = for {
    a <- this
    b <- sb
  } yield f(a, b)

  def flatMap[B](f: A => State[S, B]): State[S, B] = State(s => {
    val (a, newS) = run(s)
    f(a).run(newS)
  })
}

sealed trait Input
case object Coin extends Input
case object Turn extends Input

case class Machine(locked: Boolean, candies: Int, coins: Int)

object State {
  type Rand[A] = State[RNG, A]

  def unit[S, A](a: A): State[S, A] = State(s => (a, s))

  def sequence[S, A](fs: List[State[S, A]]): State[S, List[A]] = State(s => {
    val (results, newS) = fs.foldLeft((Nil: List[A], s)) { case ((results, currS), elem) =>
      val (a, newS) = elem.run(currS)
      (a :: results, newS)
    }
    (results.reverse, newS)
  })

  def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] = ???
}
