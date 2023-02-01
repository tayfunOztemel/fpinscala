package fpinscala.exercises.datastructures

import scala.annotation.tailrec

/** `List` data type, parameterized on a type, `A`. */
enum List[+A]:
  /** A `List` data constructor representing the empty list. */
  case Nil
  /** Another data constructor, representing nonempty lists. Note that `tail` is another `List[A]`,
    which may be `Nil` or another `Cons`.
   */
  case Cons(head: A, tail: List[A])

object List: // `List` companion object. Contains functions for creating and working with lists.
  def sum(ints: List[Int]): Int = ints match // A function that uses pattern matching to add up a list of integers
    case Nil => 0 // The sum of the empty list is 0.
    case Cons(x,xs) => x + sum(xs) // The sum of a list starting with `x` is `x` plus the sum of the rest of the list.

  def product(doubles: List[Double]): Double = doubles match
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x,xs) => x * product(xs)

  def apply[A](as: A*): List[A] = // Variadic function syntax
    if as.isEmpty then Nil
    else Cons(as.head, apply(as.tail*))

  @annotation.nowarn // Scala gives a hint here via a warning, so let's disable that
  val result = List(1,2,3,4,5) match
    case Cons(x, Cons(2, Cons(4, _))) => x
    case Nil => 42
    case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
    case Cons(h, t) => h + sum(t)
    case _ => 101

  def append[A](a1: List[A], a2: List[A]): List[A] =
    a1 match
      case Nil => a2
      case Cons(h,t) => Cons(h, append(t, a2))

  def foldRight[A,B](as: List[A], acc: B, f: (A, B) => B): B = // Utility functions
    as match
      case Nil => acc
      case Cons(x, xs) => f(x, foldRight(xs, acc, f))

  def sumViaFoldRight(ns: List[Int]): Int =
    foldRight(ns, 0, (x,y) => x + y)

  def productViaFoldRight(ns: List[Double]): Double =
    foldRight(ns, 1.0, _ * _) // `_ * _` is more concise notation for `(x,y) => x * y`; see sidebar

  def tail[A](l: List[A]): List[A] = l match {
    case List.Nil => sys.error("cannot return tail for Empty list")
    case List.Cons(_: A, t: List[A]) => t
  }

  def setHead[A](l: List[A], h: A): List[A] =
    l match
      case List.Nil => sys.error("no head in Nil")
      case List.Cons(_, tail) => List.Cons(h, tail)

  @tailrec
  def drop[A](l: List[A], n: Int): List[A] =
    l match
      case List.Nil => Nil
      case ht@List.Cons(_, _) if n <= 0 => ht
      case List.Cons(_, t) => drop(t, n - 1)

  @tailrec
  def dropWhile[A](l: List[A], f: A => Boolean): List[A] =
    l match
      case List.Nil => Nil
      case a@ List.Cons(h, t) => if !f(h) then a else dropWhile(t, f)

  /**
    * returns a List consisting of all but the last element of a List.
    * given List(1,2,3,4), init will return List(1,2,3).
    *
    * This function cannot be implemented in constant time because of the structure of a singly linked list,
    * i.e. any time we want to replace the tail of a Cons, even if it’s the last Cons in the list,
    * we must copy all the previous Cons objects.
    */
  def init[A](l: List[A]): List[A] =
    l match
      case List.Nil => sys.error("no tail for Nil")
      case List.Cons(_, Nil) => Nil
      case List.Cons(head, tail) => List.Cons(head, init(tail))

  def length[A](l: List[A]): Int =
    foldRight(l, 0, (_, t) => t+1)

  @tailrec
  def foldLeft[A, B](l: List[A], acc: B, f: (B, A) => B): B =
    l match
      case Nil => acc
      case Cons(x, xs) => foldLeft(xs, f(acc, x), f)

  def sumViaFoldLeft(ns: List[Int]): Int =
    foldLeft(ns, 0, _ + _)

  def productViaFoldLeft(ns: List[Double]): Double =
    foldLeft(ns, 1, _ * _)

  def lengthViaFoldLeft[A](l: List[A]): Int =
    foldLeft(l, 0, (x, _) => x + 1)

  def reverse[A](l: List[A]): List[A] =
    foldLeft(l, List.Nil : List[A], (x, y) => List.append(List(y), x))

  def appendViaFoldRight[A](l: List[A], r: List[A]): List[A] =
    foldRight(l, r, (a:A, b:List[A]) => List.Cons(a, b))

  def concat[A](l: List[List[A]]): List[A] =
    foldLeft(
      l,
      List.Nil : List[A],
      appendViaFoldRight(_, _)
    )

  def incrementEach(l: List[Int]): List[Int] =
    foldRight(l, List.Nil : List[Int], (a:Int, acc: List[Int]) => Cons(a+1, acc))

  def doubleToString(l: List[Double]): List[String] =
    foldRight(l, List.Nil : List[String], (a, acc) => Cons(a.toString, acc))

  def map[A, B](l: List[A], f: A => B): List[B] =
    foldRight(l, List.Nil : List[B], (a, acc) => Cons(f(a), acc))

  def filter[A](as: List[A], f: A => Boolean): List[A] =
    foldRight(as, List.Nil : List[A], (a, acc) => if f(a) then Cons(a, acc) else acc)

  def flatMap[A, B](as: List[A], f: A => List[B]): List[B] =
    foldRight(as, List.Nil: List[B], (a, acc) => appendViaFoldRight(f(a), acc))

  def filterViaFlatMap[A](as: List[A], f: A => Boolean): List[A] =
    flatMap(as, a => if f(a) then List(a) else List.Nil : List[A])

  def addPairwise(a: List[Int], b: List[Int]): List[Int] =
    (a, b) match
      case (Nil, _)=> Nil
      case (_, Nil)=> Nil
      case (Cons(ha, ta), Cons(hb, tb)) => Cons(ha+hb, addPairwise(ta, tb))

  def zipWith[A, B, C](a: List[A], b: List[B], f: (A, B) => C): List[C] =
    (a, b) match
      case (Nil, _)=> Nil
      case (_, Nil)=> Nil
      case (Cons(ha, ta), Cons(hb, tb)) => Cons(f(ha,hb), zipWith(ta, tb, f))

  def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean =
    (sup, sub) match
      case (_, Nil) => true
      case (Nil, _) => false
      case (Cons(head, tail), Cons(sh, st)) =>
        if head == sh then hasSubsequence(tail, st)
        else hasSubsequence(tail, sub)
