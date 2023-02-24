package fpinscala.exercises.state


trait RNG:
  def nextInt: (Int, RNG) // Should generate a random `Int`. We'll later define other functions in terms of `nextInt`.

object RNG:
  // NB - this was called SimpleRNG in the book text

  case class Simple(seed: Long) extends RNG:
    def nextInt: (Int, RNG) =
      val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL // `&` is bitwise AND. We use the current seed to generate a new seed.
      val nextRNG = Simple(newSeed) // The next state, which is an `RNG` instance created from the new seed.
      val n = (newSeed >>> 16).toInt // `>>>` is right binary shift with zero fill. The value `n` is our new pseudo-random integer.
      (n, nextRNG) // The return value is a tuple containing both a pseudo-random integer and the next `RNG` state.

  type Rand[+A] = RNG => (A, RNG)

  val int: Rand[Int] = _.nextInt

  def unit[A](a: A): Rand[A] =
    rng => (a, rng)

  def map[A, B](s: Rand[A])(f: A => B): Rand[B] =
    rng =>
      val (a, rng2) = s(rng)
      (f(a), rng2)

  def nonNegativeInt(rng: RNG): (Int, RNG) =
    val (r, nextRng) = rng.nextInt
    if r > Int.MinValue then (Math.abs(r), nextRng) else nonNegativeInt(nextRng)

  def double(rng: RNG): (Double, RNG) =
    val (r, rng2) = nonNegativeInt(rng)
    (r / (Int.MaxValue.toDouble + 1), rng2)

  val _double: Rand[Double] =
    map(nonNegativeInt)(i => i / (Int.MaxValue.toDouble + 1))

  def intDouble(rng: RNG): ((Int,Double), RNG) =
    val (i, rng2) = rng.nextInt
    val (d, rng3) = double(rng2)
    ((i,d), rng3)

  def doubleInt(rng: RNG): ((Double,Int), RNG) =
    val (p,r) = intDouble(rng)
    (p.swap,r)

  def double3(rng: RNG): ((Double,Double,Double), RNG) =
    val (d1, rng1) = double(rng)
    val (d2, rng2) = double(rng1)
    val (d3, rng3) = double(rng2)
    ((d1, d2, d3), rng3)

  def ints(count: Int)(rng: RNG): (List[Int], RNG) =
    intsViaTailRecursion(count)(rng)

  private def intsViaSimpleRecursion(count: Int)(rng: RNG): (List[Int], RNG) =
    if(count > 0) then
      val (i1, r1) = rng.nextInt
      val (ii, rr) = ints(count -1)(r1)
      (i1 :: ii, rr)
    else
      (Nil, rng)

  private def intsViaTailRecursion(count: Int)(rng: RNG): (List[Int], RNG) =
    def go(c: Int, r: RNG, is: List[Int]): (List[Int], RNG) =
      if c <= 0 then
        (is, r)
      else
        val (i, r1) = r.nextInt
        go(c - 1, r1, i :: is)

    go(count, rng, List())

  private def intsViaSequence(count: Int): Rand[List[Int]] =
    sequence(List.fill(count)(int))

  def map2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
    (r0:RNG) => {
      val (a, r1) = ra (r0)
      val (b, r2) = rb (r1)
      (f(a, b), r2)
    }

  def both[A, B](ra: Rand[A], rb: Rand[B]): Rand[(A, B)] =
    map2(ra, rb)((_,_))

  val randIntDouble: Rand[(Int, Double)] = both(int, double)

  val randDoubleInt: Rand[(Double, Int)] = both(double, int)

  def sequence[A](rs: List[Rand[A]]): Rand[List[A]] =
    rs.foldRight(unit(Nil:List[A])){
      case (accR, aR) => map2(accR, aR)(_ :: _)
    }

  def flatMap[A, B](r: Rand[A])(f: A => Rand[B]): Rand[B] =
    (rng: RNG) => {
      val (v, rng2) = r(rng)
      f(v)(rng2)
    }

  def nonNegativeLessThan(n: Int): Rand[Int] =
    flatMap(nonNegativeInt){a =>
      val mod = a % n
      if a + (n-1) - mod >= 0 then unit(mod)
      else nonNegativeLessThan(n)
    }

  def mapViaFlatMap[A, B](r: Rand[A])(f: A => B): Rand[B] =
    flatMap(r)(f.andThen(unit))

  def map2ViaFlatMap[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
    flatMap(ra)(a => mapViaFlatMap(rb)(b => f(a,b)))

opaque type State[S, +A] = S => (A, S)

object State:

  extension [S, A](underlying: State[S, A])
    def run(s: S): (A, S) = underlying(s)

    def flatMap[B](f: A => State[S, B]): State[S, B] =
      (s: S) =>
        val (a, sa) = run(s)
        f(a)(sa)

    def map[B](f: A => B): State[S, B] = flatMap(f.andThen(unit))

    def map2[B, C](sb: State[S, B])(f: (A, B) => C): State[S, C] = flatMap(a => sb.map(b => f(a,b)))

  def apply[S, A](f: S => (A, S)): State[S, A] = f

  def unit[S, A](a: A): State[S, A] = apply(s => (a, s))

  def sequence[S,A] (ss: List[State[S, A]]): State[S, List[A]] =
    ss.foldRight(unit(Nil: List[A])) ((a, acc) => a.map2(acc)(_ :: _))

  def get[S]: State[S,S] = s => (s, s)

  def set[S](s: S): State[S, Unit] = _ => ((), s)

  def modify[S](f: S => S): State[S, Unit] =
    for {
      s <- get
      _ <- set(f(s))
    } yield ()

enum Input:
  case Coin, Turn

case class Machine(locked: Boolean, candies: Int, coins: Int) {
  def turn() = this match
    case Machine(false, ca, co) if ca > 0 => Machine(true, ca - 1, co)
    case _ => this
  def insert() = this match
    case Machine(true, ca, co) if ca > 0 => Machine(false, ca, co+1)
    case _ => this

}

object Candy:
  def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] =
    for
      _ <- State.sequence(inputs.map(i => State.modify[Machine](update(i, _))))
      s <- State.get
    yield (s.coins, s.candies)

  def simulateMachineOld(inputs: List[Input]): State[Machine, (Int, Int)] =
    inputs match
      case ::(head, next) =>
        val s1: State[Machine, (Int, Int)] = head match
          case Input.Coin =>
            State(m => {
              val mm = m.insert()
              ((mm.coins, mm.candies), mm)
            })
          case Input.Turn =>
            State(m => {
              val mm = m.turn()
              ((mm.coins, mm.candies), mm)
            })
        val s2: State[Machine, (Int, Int)] = simulateMachine(next)
        s1.flatMap(cc => s2)
      case Nil => State(m => ((m.coins, m.candies), m))

  def update(i: Input, s: Machine): Machine =
    (i, s) match
      case (Input.Coin, Machine(true, ca, co)) if ca > 0 => Machine(false, ca, co + 1)
      case (Input.Turn, Machine(false, ca, co)) if ca > 0 => Machine(true, ca - 1, co)
      case (_, m) => m

