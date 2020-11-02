package RedBook

import RedBook.PropImplementation.{Gen, Prop}
import RedBook.PropImplementation.Prop._

import scala.util.matching.Regex
/*
object ParserCombinators {

  type ParseError = String
  type Parser[+A] = String => Either[ParseError,A]

  trait Parsers { self =>
    def run[A](p: Parser[A])(input: String): Either[ParseError,A] = p(input)
    def char(c: Char): Parser[Char] = s => {
      val firstChar = s.toCharArray.headOption
      firstChar match {
        case Some(ch) => if (ch == c) Right(c) else Left(s"Expected '$c', found '$s'")
        case None => Left(s"Expected '$c', found empty string")
      }
    }
    private def combineErrors(err1: ParseError, err2: ParseError): ParseError = s"$err1\n$err2"
    def or[A](s1: Parser[A], s2: Parser[A]): Parser[A] = input =>
      run(s1)(input) match {
        case Left(err1) => run(s2)(input) match {
          case Left(err2) => Left(combineErrors(err1,err2))
          case x => x
        }
        case x => x
      }
    def map2[A,B,C](s1: Parser[A],s2: Parser[B])(f: (A,B) => C): Parser[C] = s1.flatMap(a => s2.map(b => f(a,b)))
    def map[A,B](p: Parser[A])(f: A => B): Parser[B] = input =>
      run(p)(input) match {
        case Right(v) => Right(f(v))
        case Left(err) => Left(err)
      }
    def flatMap[A,B](p: Parser[A])(f: A => Parser[B]): Parser[B] = input =>
      run(p)(input) match {
        case Right(v) => run(f(v))(input)
        case Left(err) => Left(err)
      }
    def sequence[A](l: List[Parser[A]]): Parser[List[A]] = s =>
      l.foldLeft(_: Parser[List[A]])((pAcc,pa) => pAcc.map2(pa))

    def listOfN[A](n: Int, p: Parser[A]): Parser[List[A]]

    implicit def string(s: String): Parser[String] = _ => Right(s)
    //implicit def any[A](x: A): Parser[A]
    implicit def operators[A](p: Parser[A]): ParserOps[A] = ParserOps[A](p)
    implicit def asStringParser[A](a: A)(implicit f: A => Parser[String]): ParserOps[String] = ParserOps(f(a))

    case class ParserOps[A](p: Parser[A]) {
      //def run(s: String): Either[ParseError, A] = self.run(p)(s)
      def |[B>:A](p2: Parser[B]): Parser[B] = self.or(p,p2)
      def or[B>:A](p2: => Parser[B]): Parser[B] = self.or(p,p2)
      def map[B](f: A => B): Parser[B] = self.map(p)(f)
      def flatMap[B](f: A => Parser[B]): Parser[B] = self.flatMap(p)(f)
      def map2[B,C](p2: Parser[B])(f: (A,B) => C): Parser[C] = self.map2(p,p2)(f)
    }

    /**
     * A Parser[Int] that recognizes zero or more 'a' characters, and whose result
     * value is the number of 'a' characters it has seen. For instance, given "aa" , the
     * parser results in 2 ; given "" or "b123" (a string not starting with 'a' ), it results
     * in 0 ; and so on.
     */
    def occurrences(regex: Regex): Parser[Int] = Parser(s => {
      Right((regex findAllIn s).length)
    })
    def charOccurrences(c: Char): Parser[Int] = occurrences(s"$c*".r)
    def charOccurrencesAtStart(c: Char): Parser[Int] = occurrences(s"^$c*".r)

    /**
     * A Parser[Int] that recognizes one or more 'a' characters, and whose result
     * value is the number of 'a' characters it has seen. (Is this defined somehow in
     * terms of the same combinators as the parser for 'a' repeated zero or more
     * times?) The parser should fail when given a string without a starting 'a' . How
     * would you like to handle error reporting in this case? Could the API support giv-
     * ing an explicit message like "Expected one or more 'a'" in the case of failure?
     */
    def occurrencesOrError(regex: Regex): Parser[Int] = occurrences(regex).flatMap(n =>
      Parser(_ =>
        if (n == 0) Left(s"Expected one or more occurrences matching '$regex'") else Right(n)
      )
    )

    /**
     * A parser that recognizes zero or more 'a' , followed by one or more 'b' , and
     * which results in the pair of counts of characters seen. For instance, given "bbb" ,
     * we get (0,3) , given "aaaab" , we get (4,1) , and so on.
     */
    def occurrenceList(regexList: List[Regex]): Parser[List[Int]] = Parser(s =>
      regexList.map(regex =>
        run(occurrences(regex))(s)
      )
    )
    def charOccurrenceList(chList: List[Char]): Parser[List[Int]] = ???

  }
  /*
  object Laws {
    def equal[A](p1: Parser[A], p2: Parser[A])(in: Gen[String]): Prop =
      forAll(in)(s => run(p1)(s) == run(p2)(s))
    def mapLaw[A](p: Parser[A])(in: Gen[String]): Prop =
      equal(p, p.map(a => a))(in)
  }
  */
}
*/