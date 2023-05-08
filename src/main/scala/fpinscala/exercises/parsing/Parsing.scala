package fpinscala.exercises.parsing

object Parsing {

  trait Parsers[ParseError, Parser[+_]]:
    def char(c: Char): Parser[Char] =
      string(c.toString).map(_.charAt(0))
    def succeed[A](a: A): Parser[A] =
      string("").map(_ => a)
    def string(s: String): Parser[String]

    def map2[A, B, C](p:Parser[A], p2: Parser[B])(f: (A, B) => C): Parser[C] =
      map(product(p, p2))(f.tupled)
    def many1[A](p:Parser[A]): Parser[List[A]] =
      map2(p, many(p))(_ :: _)

    object Laws:
      def charLaw(in: Gen[Char]): Prop =
        Prop.forAll(in)(c => char(c).run(c.toString) == Right(c))

      def stringLaw(in: Gen[String]): Prop =
        Prop.forAll(in)(s => string(s).run(s) == Right(s))

      def equal[A](p1:Parser[A], p2: Parser[A])(in: Gen[String]): Prop =
        Prop.forAll(in)(s => p1.run(s) == p2.run(s))

      def mapLaw[A](p: Parser[A])(in: Gen[String]): Prop =
        equal(p, p.map(a => a))(in)

      def succeedLaw(a: A)(in: Gen[String]): Prop =
        Prop.forAll(in)(s => succeed(a).run(s) == Right(a))

      def orLaw(a: A)(in1: Gen[String], in2: Gen[String]): Prop =
        Prop.forAll(in1, in2)((s1, s2) => {
          string(s1).or(string(s2)).run(s1) == Right(s1)
          string(s1).or(string(s2)).run(s2) == Right(s2)
        })

    extension [A](p: Parser[A])
      def run(input: String): Either[ParseError, A]
      infix def or(p2: Parser[A]): Parser[A]
      def |(p2: Parser[A]): Parser[A] = p.or(p2)
      def listOfN(n: Int): Parsuer[List[A]]
      def oneOrMore: Parser[Int]
      def map[B](f: A=>B): Parser[B]
      def many: Parser[List[Int]]
      def manyOne : Parser[Int] =
        p.product(p.many)
      def slice: Parser[String]
      def product[B](p2: Parser[B]): Parser[(A, B)]
      def **[B](p2: Parser[B]): Parser[(A, B)] = product(p2)
//      def map2[B, C](p2: Parser[B])(f: (A, B) => C): Parser[C] =
//        map(p.product(p2))(f.tupled)

}
