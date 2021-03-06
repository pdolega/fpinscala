package fpinscala.datastructures

import scala.annotation.tailrec
import scala.collection.mutable.ListBuffer

sealed trait List[+A] // `List` data type, parameterized on a type, `A`
case object Nil extends List[Nothing] // A `List` data constructor representing the empty list
case class Cons[+A](head: A, tail: List[A]) extends List[A] // Another data constructor, representing nonempty lists. Note that `tail` is another `List[A]`, which may be `Nil` or another `Cons`.

object List { // `List` companion object. Contains functions for creating and working with lists.
  def sum(ints: List[Int]): Int = ints match { // A function that uses pattern matching to add up a list of integers
    case Nil => 0 // The sum of the empty list is 0.
    case Cons(x,xs) => x + sum(xs) // The sum of a list starting with `x` is `x` plus the sum of the rest of the list.
  }

  def sumFoldLeft(ints: List[Int]): Int = foldLeft(ints, 0)(_ + _)
  
  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x,xs) => x * product(xs)
  }

  def productFoldLeft(ds: List[Double]): Double = foldLeft(ds, 1.0)(_ * _)
  
  def apply[A](as: A*): List[A] = // Variadic function syntax
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  val x = List(1,2,3,4,5) match {
    case Cons(x, Cons(2, Cons(4, _))) => x
    case Nil => 42 
    case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
    case Cons(h, t) => h + sum(t)
    case _ => 101 
  }

  def append[A](a1: List[A], a2: List[A]): List[A] =
    a1 match {
      case Nil => a2
      case Cons(h,t) => Cons(h, append(t, a2))
    }

  def foldRight[A,B](as: List[A], z: B)(f: (A, B) => B): B = // Utility functions
    as match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }
  
  def sum2(ns: List[Int]) = 
    foldRight(ns, 0)((x,y) => x + y)
  
  def product2(ns: List[Double]) = 
    foldRight(ns, 1.0)(_ * _) // `_ * _` is more concise notation for `(x,y) => x * y`; see sidebar


  def tail[A](l: List[A]): List[A] = l match {
    case Nil => sys.error("Taking tail of an empty list")
    case Cons(_, t) => t
  }

  def setHead[A](l: List[A], h: A): List[A] = l match {
    case Nil => sys.error("Unable to replace head on empty list")
    case Cons(_, t) => Cons(h, t)
  }

  @tailrec
  def drop[A](l: List[A], n: Int): List[A] = {
    require(n >= 0, "Number of elements to drop must be nonnegative")

    if(n > 0) {
      l match {
        case Nil => sys.error("Number of elements to drop bigger than the size of the list")
        case Cons(_, t) => drop(t, n - 1)
      }
    } else {
      l
    }
  }

  @tailrec
  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = l match {
    case Nil => Nil
    case Cons(h, t) => if(f(h)) dropWhile(t,f) else Cons(h, t)
  }

  def init[A](l: List[A]): List[A] = {
    l match {
      case Nil => sys.error("init cannot be called on empty list")
      case Cons(_, Nil) => Nil
      case Cons(h, t) => Cons(h, init(t))
    }
  }

  def length[A](l: List[A]): Int = foldRight(l, 0)((_, acc) => acc + 1)

  def lengthFoldLeft[A](l: List[A]): Int = foldLeft(l, 0)((acc, _) => acc + 1)

  @tailrec
  def foldLeft[A,B](l: List[A], z: B)(f: (B, A) => B): B = {
    l match {
      case Nil => z
      case Cons(h, t) => foldLeft(t, f(z, h))(f)
    }
  }

  def reverse[A](l: List[A]): List[A] = foldLeft(l, List[A]())((reversed, elem) => Cons(elem, reversed))

  def foldLeftWithFoldRight[A, B](l: List[A], z: B)(f: (B, A) => B): B = {
    foldRight(reverse(l), z)((elem,acc) => f(acc, elem))
  }

  def foldRightWithFoldLeft[A, B](l: List[A], z: B)(f: (A, B) => B): B = {
    foldLeft(reverse(l), z)((acc,elem) => f(elem, acc))
  }

  def foldRightWithFoldLeft1[A,B](l: List[A], z: B)(f: (A,B) => B): B =
    foldLeft(l, (b: B) => b)( (acc: B => B, elem: A) => (b: B) => acc(f(elem,b)) )(z)

  def appendFold[A](a1: List[A], a2: List[A]): List[A] = foldRight(a1, a2)(Cons(_, _))

  def concatenate[A](l: List[List[A]]): List[A] = foldRight(l, List[A]())(append)

  def mapAdd1ToEach(l: List[Int]): List[Int] = l match {
    case Nil => Nil
    case Cons(h, t) => Cons(h + 1, mapAdd1ToEach(t))
  }

  def mapToString(l: List[Double]): List[String] = foldRight(l, List[String]())((elem, acc) => Cons(elem.toString, acc))

  def map[A,B](l: List[A])(f: A => B): List[B] = foldRight(l, List[B]()){ (elem, acc) => Cons(f(elem), acc) }

  // wanted to skip foldRight this time
  def filter[A](as: List[A])(f: A => Boolean): List[A] = {
    val buffer = new ListBuffer[A]()

    @tailrec
    def fillBuff(as: List[A])(f: A => Boolean) {
      as match {
        case Nil => ()
        case Cons(h, t) => {
          if(f(h)) buffer += h
          fillBuff(t)(f)
        }
      }
    }

    fillBuff(as)(f)
    List(buffer.toList: _*)
  }

  def flatMap[A, B](as: List[A])(f: A => List[B]): List[B] = concatenate(map(as)(f))

  def filterWithFlatmap[A](as: List[A])(f: A => Boolean): List[A] = flatMap(as){ x => if(f(x)) List(x) else Nil }

  def zipByAdding(list1: List[Int], list2: List[Int]): List[Int] = zipWith(list1, list2) { _ + _ }

  def zipWith[A, B, C](a: List[A], b: List[B])(f: (A, B) => C): List[C] = {
    val buffer = new ListBuffer[C]

    @tailrec
    def fillBuffer(list1: List[A], list2: List[B]): Unit = {
      (list1, list2) match {
        case (Nil, Nil) => ()
        case (Cons(h1, t1), Cons(h2, t2)) => {
          buffer += f(h1, h2)
          fillBuffer(t1, t2)
        }
        case _ => sys.error("Lists passed as params are not of equal length")
      }
    }

    fillBuffer(a, b)
    List(buffer.toList: _*)
  }

  @tailrec
  def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean = {

    @tailrec
    def startsWith(list: List[A], inner: List[A]): Boolean = (list, inner) match {
      case (_, Nil) => true
      case (Cons(h1, t1), Cons(h2, t2)) if h1 == h2 => startsWith(t1, t2)
      case _ => false
    }

    sup match {
      case list if startsWith(list, sub) => true
      case Cons(h, t) => hasSubsequence(t, sub)
      case _ => false
    }
  }
}