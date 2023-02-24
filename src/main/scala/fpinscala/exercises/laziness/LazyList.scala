package fpinscala.exercises.laziness

import LazyList.*

enum LazyList[+A]:
  case Empty
  case Cons(h: () => A, t: () => LazyList[A])

  def toList: List[A] = this match
    case Empty => Nil
    case Cons(h, t) => h() :: t().toList

  def foldRight[B](z: => B)(f: (A, => B) => B): B = // The arrow `=>` in front of the argument type `B` means that the function `f` takes its second argument by name and may choose not to evaluate it.
    this match
      case Cons(h,t) => f(h(), t().foldRight(z)(f)) // If `f` doesn't evaluate its second argument, the recursion never occurs.
      case _ => z

  /**
    * Here `b` is the unevaluated recursive step that folds the tail of
    * the lazy list. If `p(a)` returns `true`, `b` will never be evaluated
    * and the computation terminates early.
   */
  def exists(p: A => Boolean): Boolean =
    foldRight(false)((a, b) => p(a) || b)

  @annotation.tailrec
  final def find(f: A => Boolean): Option[A] = this match
    case Empty => None
    case Cons(h, t) => if (f(h())) Some(h()) else t().find(f)

  def take(n: Int): LazyList[A] = takeViaUnfold(n)

  private def takeViaPatternMatch(n: Int): LazyList[A] = this match
    case Cons(h, t) if n > 0 => Cons(() => h(), () => t().take(n-1))
    case _ => empty

  def takeViaUnfold(n: Int): LazyList[A] =
    unfold((this, n)) {
      case (Cons(h, t), s) if s > 0 => Some((h(), (t(), s-1)))
      case _ => None
    }

  def drop(n: Int): LazyList[A] = this match
    case Cons(_, t) if n > 0 => t().drop(n-1)
    case _ => this

  def takeWhile(p: A => Boolean): LazyList[A] =
    takeWhileViaUnfold(p)

  private def takeWhileViaFoldRight(p: A => Boolean): LazyList[A] =
    foldRight(empty)((a, acc) => if p(a) then cons(a, acc) else empty)

  private def takeWhileViaPatternMatch (p: A => Boolean): LazyList[A] = this match
    case Cons(h, t) if p(h()) => cons(h(), t().takeWhile(p))
    case _ => empty

  def takeWhileViaUnfold (p: A => Boolean): LazyList[A] =
    unfold(this) {
      case Cons(h, t) if p(h()) => Some(h(), t())
      case _ => None
    }

  def forAll(p: A => Boolean): Boolean =
    foldRight(true)((a, acc) => p(a) && acc)

  def headOption: Option[A] =
    headOption_foldRight

  private def headOption_foldRight: Option[A] =
    foldRight(None:Option[A])((a, _) => Option(a))

  private def headOption_match: Option[A] =
    this match
      case Empty => None
      case Cons(h, t) => Some(h())

  // 5.7 map, filter, append, flatmap using foldRight. Part of the exercise is
  // writing your own function signatures.

  def startsWithViaPatternMatch[B](s: LazyList[B]): Boolean = {
    def go(s1: LazyList[B]) =
      (this, s1) match
        case (_, Empty) => true
        case (Empty, _) => false
        case (Cons(h, t), Cons(h2, t2)) =>
          if h == h2 then t().startsWithViaPatternMatch(t2()) else t().startsWithViaPatternMatch(s)

    go(s)
  }

  def startsWith[A2 >: A](prefix: LazyList[A2]): Boolean =
    zipAll(prefix).takeWhile(_(1).isDefined).forAll{ case (a1, a2) => a1 == a2}

  def map[B](f: A => B): LazyList[B] = mapViaUnfold(f)

  private def mapViaFoldRight[B](f: A => B): LazyList[B] =
    foldRight(empty)((a, acc) => cons(f(a), acc))

  def mapViaUnfold[B](f: A => B): LazyList[B] =
    unfold(this) {
      case Empty => None
      case Cons(h, t) => Some(f(h()), t())
    }

  def filter(f: A => Boolean): LazyList[A] =
    foldRight(empty[A])((a, acc) => if f(a) then cons(a, acc) else acc)

  def append[A2 >: A](list: => LazyList[A2]): LazyList[A2] =
    foldRight(list)((a, acc) => cons(a, acc))

  def flatMap[B](f: A => LazyList[B]): LazyList[B] =
    foldRight(empty[B])((a, acc) => f(a).append(acc))

  def zipWith[A2 >: A, B](other: LazyList[A2])(f: (A, A2) => B): LazyList[B] =
    unfold((this, other)) {
      case (Cons(h1, t1), Cons(h2, t2)) => Some(f(h1(), h2()), (t1(), t2()))
      case _ => None
    }

  def zipAll[B](that: LazyList[B]): LazyList[(Option[A], Option[B])] =
    unfold((this, that)) {
      case (Cons(h1, t1), Cons(h2, t2)) => Some((Some(h1()), Some(h2())), (t1(), t2()))
      case (Cons(h1, t1), Empty) => Some((Some(h1()), None), (t1(), Empty))
      case (Empty, Cons(h2, t2)) => Some((None, Some(h2())), (Empty, t2()))
      case _ => None
    }

  def tails: LazyList[LazyList[A]] =
    unfold(this){
      case Empty => None
      case Cons(h, t) => Some((Cons(h, t), t()))
    }.append(LazyList(empty))

  def hasSubsequence[B](that: LazyList[B]): Boolean =
    tails.exists(_.startsWith(that))

  def scanRight[B](acc: B)(f: (A, =>B)=>B): LazyList[B] =
    def go(l: LazyList[A]): (B, LazyList[B]) =
      l match
        case LazyList.Empty => (acc, LazyList(acc))
        case LazyList.Cons(h, t) =>
          val (sum, list) = go(t())
          val newSum = f(h(), sum)
          (newSum, LazyList(newSum).append(list))
    go(this)._2

  def scanRightViaFold[B](acc: B)(f: (A, =>B)=>B): LazyList[B] =
    foldRight(acc -> LazyList(acc)) {(a, b0) =>
      lazy val b1 = b0
      val b2 = f(a, b1(0))
      (b2, cons(b2, b1(1)))
    }(1)

object LazyList:
  def cons[A](hd: => A, tl: => LazyList[A]): LazyList[A] = 
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)

  def empty[A]: LazyList[A] = Empty

  def apply[A](as: A*): LazyList[A] =
    if as.isEmpty then empty 
    else cons(as.head, apply(as.tail*))

  val ones: LazyList[Int] = LazyList.cons(1, ones)

  def continually[A](a: A): LazyList[A] =
    lazy val l = cons(a, continually(a))
    l

  def from(n: Int): LazyList[Int] =
    cons(n, from(n+1))

  lazy val fibs: LazyList[Int] = {
    def go(current: Int, next:Int): LazyList[Int] =
      cons(current, go(next, current+next))
    go(0, 1)
  }

  def unfold[A, S](state: S)(f: S => Option[(A, S)]): LazyList[A] = {
    f(state) match
      case Some((h, s)) => cons(h, unfold(s)(f))
      case None => empty[A]
  }

  lazy val fibsViaUnfold: LazyList[Int] =
    unfold((0, 1)) { case (s1, s2) => Some((s1, (s2, s1 + s2))) }

  def fromViaUnfold(n: Int): LazyList[Int] =
    unfold(n)(s => Some((s, s+1)))

  def continuallyViaUnfold[A](a: A): LazyList[A] =
    unfold(())(_ => Some((a, ())))

  lazy val onesViaUnfold: LazyList[Int] =
    unfold(())(_ => Some((1, ())))

