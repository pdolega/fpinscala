package fpinscala.laziness

import Stream._

import scala.annotation.tailrec

trait Stream[+A] {

  def foldRight[B](z: => B)(f: (A, => B) => B): B = // The arrow `=>` in front of the argument type `B` means that the function `f` takes its second argument by name and may choose not to evaluate it.
    this match {
      case Cons(h,t) => f(h(), t().foldRight(z)(f)) // If `f` doesn't evaluate its second argument, the recursion never occurs.
      case _ => z
    }

  def exists(p: A => Boolean): Boolean = 
    foldRight(false)((a, b) => p(a) || b) // Here `b` is the unevaluated recursive step that folds the tail of the stream. If `p(a)` returns `true`, `b` will never be evaluated and the computation terminates early.

  @annotation.tailrec
  final def find(f: A => Boolean): Option[A] = this match {
    case Empty => None
    case Cons(h, t) => if (f(h())) Some(h()) else t().find(f)
  }

  def toList: List[A] = foldRight(List[A]()) { _ :: _ }

  def take(n: Int): Stream[A] = {
    @tailrec
    def takeInner(built: List[A], rest: Stream[A], currN: Int): List[A] = rest match {
      case Empty => built
      case Cons(h, tail) => if(currN > 0) takeInner(h() :: built, tail(), currN - 1) else built
    }

    Stream(takeInner(Nil, this, n).reverse : _*)
  }

  @tailrec
  final def drop(n: Int): Stream[A] = this match {
    case Empty => empty
    case Cons(h, tail) => if (n > 0) tail().drop(n - 1) else Cons(h, tail)
  }

  def takeWhile(p: A => Boolean): Stream[A] = {
    @tailrec
    def takeInner(built: List[A], rest: Stream[A]): List[A] = rest match {
      case Empty => built
      case Cons(h, tail) => if(p(h())) takeInner(h() :: built, tail()) else built
    }

    Stream(takeInner(Nil, this).reverse : _*)
  }

  def takeWhileFoldRight(p: A => Boolean): Stream[A] = {
    foldRight(Empty: Stream[A]) { (elem, acc) =>
      if (p(elem)) {
        cons(elem, acc)
      } else {
        Empty
      }
    }
  }

  def forAll(p: A => Boolean): Boolean = {
    foldRight(true) {(elem, acc) =>
      p(elem) && acc
    }
  }

  def headOption: Option[A] = foldRight(None: Option[A]) { (elem, _) => Some(elem) }

  def map[B](f: A => B): Stream[B] = foldRight(Empty: Stream[B]) { (elem, acc) => cons(f(elem), acc) }

  def filter(f: A => Boolean): Stream[A] = foldRight(Stream[A]()) { (elem, acc) =>
    if(f(elem)) {
      cons(elem, acc)
     } else {
      acc
    }
  }

  def append[V >: A](other: => Stream[V]): Stream[V] = foldRight(other) { (elem, acc) => cons(elem, acc) }

  def flatMap[B](f: A => Stream[B]): Stream[B] = foldRight(Empty: Stream[B]) { (elem, acc) => f(elem).append(acc)  }

  def startsWith[B](s: Stream[B]): Boolean = sys.error("todo")
}
case object Empty extends Stream[Nothing]
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {
  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }

  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty 
    else cons(as.head, apply(as.tail: _*))

  val ones: Stream[Int] = Stream.cons(1, ones)
  val onesUnfold: Stream[Int] = unfold(1) { state => Option((1, state)) }

  def constant[A](a: A) : Stream[A] = cons(a, constant(a))
  def constantUnfold[A](a: A) : Stream[A] = unfold(a) { state => Option((state, state)) }

  def from(n: Int): Stream[Int] = cons(n, from(n + 1))
  def fromUnfold(n: Int) : Stream[Int] = unfold(n) { state => Option((state, state + 1)) }

  def fibs: Stream[Int] = {
    def innerGeneration(current: Int, next: Int): Stream[Int] = {
      cons(current, innerGeneration(next, current + next))
    }

    innerGeneration(1, 1)
  }
  def fibsUnfold: Stream[Int] = unfold((1, 1)) { state => Option(state._1, (state._2, state._1 + state._2)) }

  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = {
    val elemAndState = f(z)
    elemAndState match {
      case None => Empty
      case Some((elem, state)) => cons(elem, unfold(state)(f))
    }
  }
}