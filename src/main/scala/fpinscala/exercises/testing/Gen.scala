package fpinscala.exercises.testing

import fpinscala.exercises.state.*
import fpinscala.exercises.parallelism.*
import fpinscala.exercises.parallelism.Par.Par
import Gen.{listOfN, *}
import Prop.*

import java.util.concurrent.{ExecutorService, Executors}

enum Result :
  case Passed
  case Falsified(failure: FailedCase, successes: SuccessCount)

  def isFalsified: Boolean = this match
    case Passed => false
    case Falsified(_, _) => true

opaque type TestCases = Int
object TestCases:
  extension (x: TestCases) def toInt: Int = x
  def fromInt(x: Int): TestCases = x

opaque type Prop =
  (TestCases, RNG) => Result

object Prop:

  import Result.*

  opaque type SuccessCount = Int
  opaque type FailedCase = String

  def forAll[A](as: Gen[A])(f: A => Boolean): Prop =
    (n, rng) =>
      randomLazyList(as)(rng)
        .zip(LazyList.from(0))
        .take(n)
        .map {
          case (a, i) =>
            try
              if f(a) then Passed else Falsified(a.toString, i)
            catch
              case e: Exception => Falsified(buildMsg(a, e), i)
        }
        .find(_.isFalsified)
        .getOrElse(Passed)

  def randomLazyList[A](g: Gen[A])(rng: RNG): LazyList[A] =
    LazyList.unfold(rng)(rng => Some(g.run(rng)))

  def buildMsg[A](s: A, e: Exception): String =
    s"""
      |test case: $s\n
      |generated an exception: ${e.getMessage}\n
      |stack trace:\n ${e.getStackTrace.mkString("\n")}
      |""".stripMargin

  extension (self:Prop) def &&(that: Prop): Prop = ???
  extension (self:Prop) def ||(that: Prop): Prop = ???

opaque type Gen[+A] = State[RNG, A]

object Gen:
  def unit[A](a: => A): Gen[A] =
    State.unit(a)

  def boolean: Gen[Boolean] =
    State(RNG.boolean)

  def choose(start: Int, stopExclusive: Int): Gen[Int] =
    State(RNG.nonNegativeInt).map(i => start + i % (stopExclusive - start))

  def union[A](g1:Gen[A], g2:Gen[A]): Gen[A] =
    boolean.flatMap(b => if b then g1 else g2)

  def weighted[A](g1:(Gen[A], Double), g2:(Gen[A], Double)): Gen[A] =
    val threshold = g1._2.abs / (g1._2.abs + g2._2.abs)
    State(RNG.double).flatMap(d => if d < threshold then g1._1 else g2._1)

  extension [A](self: Gen[A])
    def next(rng: RNG): (A, RNG) = self.run(rng)

    def listOfN(size: Gen[Int]): Gen[List[A]] =
      size.flatMap(listOfN)

    def listOfN(n: Int): Gen[List[A]] = State.sequence(List.fill(n)(self))
    def flatMap[B](f: A => Gen[B]): Gen[B] =
      State.flatMap(self)(f)


//trait Gen[A]:
//  def map[B](f: A => B): Gen[B] = ???
//  def flatMap[B](f: A => Gen[B]): Gen[B] = ???

trait SGen[+A]
