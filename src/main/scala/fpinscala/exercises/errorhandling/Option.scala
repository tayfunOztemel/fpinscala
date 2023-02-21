package fpinscala.exercises.errorhandling

// Hide std library `Option` since we are writing our own in this chapter
import scala.{Option as _, Some as _, None as _}

enum Option[+A]:
  case Some(get: A)
  case None

  def map[B](f: A => B): Option[B] = this match
    case None => None
    case Some(a) => Some(f(a))

  def getOrElse[B>:A](default: => B): B = this match
    case None => default
    case Some(a) => a

  def flatMap[B](f: A => Option[B]): Option[B] =
    map(f).getOrElse(None)

  def orElse[B>:A](ob: => Option[B]): Option[B] =
    map(Some(_)).getOrElse(ob)

  def filter(f: A => Boolean): Option[A] =
    flatMap(a => if f(a) then Some(a) else None)

object Option:

  def failingFn(i: Int): Int =
    // `val y: Int = ...` declares `y` as having type `Int`, and sets it equal to the right hand side of the `=`.
    val y: Int = throw new Exception("fail!")
    try
      val x = 42 + 5
      x + y
    // A `catch` block is just a pattern matching block like the ones we've seen. `case e: Exception` is a pattern
    // that matches any `Exception`, and it binds this value to the identifier `e`. The match returns the value 43.
    catch case e: Exception => 43

  def failingFn2(i: Int): Int =
    try
      val x = 42 + 5
      // A thrown Exception can be given any type; here we're annotating it with the type `Int`
      x + ((throw new Exception("fail!")): Int)
    catch case e: Exception => 43

  def mean(xs: Seq[Double]): Option[Double] =
    if xs.isEmpty then None
    else Some(xs.sum / xs.length)

  def variance(xs: Seq[Double]): Option[Double] =
    mean(xs).flatMap(m => mean(xs.map(x => math.pow(x - m, 2))))

  def map2[A,B,C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] =
    a.flatMap(aa => b.map(bb => f(aa, bb)))

  def sequence[A](as: List[Option[A]]): Option[List[A]] =
    sequence_3(as)

  private def sequence_3[A](as: List[Option[A]]): Option[List[A]] =
    traverse(as)(as => as)

  private def sequence_1[A](as: List[Option[A]]): Option[List[A]] =
    as.foldLeft(Some(List.empty[A])){
      case (Some(acc), Some(a))=> Some(acc:+a)
      case _ => None
    }

  private def sequence_2[A](as: List[Option[A]]): Option[List[A]] =
    as match
      case Nil => Some(Nil)
      case ::(head, tail) => head.flatMap(hh => sequence_2(tail).map(hh +: _))

  def traverse[A, B](as: List[A])(f: A => Option[B]): Option[List[B]] =
    traverse_1(as)(f)

  private def traverse_2[A, B](as: List[A])(f: A => Option[B]): Option[List[B]] =
    as.foldLeft(Some(List.empty[B])) {
      case (Some(acc), a) => f(a).map(b => acc :+ b)
      case _ => None
    }

  private def traverse_1[A, B](as: List[A])(f: A => Option[B]): Option[List[B]] =
    as match
      case Nil => Some(Nil)
      case ::(head, next) => map2(f(head), traverse_1(next)(f))(_ +: _)

  def lift[A, B](f: A => B): Option[A] => Option[B] =
    _.map(f)