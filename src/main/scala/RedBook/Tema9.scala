package RedBook

import RedBook.PropImplementation.{Gen, Prop}
import RedBook.PropImplementation.Prop._

import scala.language.implicitConversions

import scala.util.matching.Regex

object ParserCombinators {

  trait Parsers[ParseError,Parser[+_]] { self =>
    def run[A](p: Parser[A])(input: String): Either[ParseError,A]
    def unit[A](a: A): Parser[A]
    def error(s: String): ParseError
    def char(c: Char): Parser[Char]
    def or[A](s1: Parser[A], s2: Parser[A]): Parser[A]
    def map2[A,B,C](s1: Parser[A],s2: Parser[B])(f: (A,B) => C): Parser[C] = s1.flatMap(a => s2.map(b => f(a,b)))
    def map[A,B](p: Parser[A])(f: A => B): Parser[B] = p.flatMap(a => unit(f(a)))
    def flatMap[A,B](p: Parser[A])(f: A => Parser[B]): Parser[B]
    def sequence[A](l: List[Parser[A]]): Parser[List[A]] =
      l.foldLeft(unit(List(): List[A]))((acc,p) => map2(acc,p)((l,pr) => pr :: l))
    def listOfN[A](n: Int, p: Parser[A]): Parser[List[A]]

    implicit def string(s: String): Parser[String]
    implicit def operators[A](p: Parser[A]): ParserOps[A] = ParserOps[A](p)
    implicit def asStringParser[A](a: A)(implicit f: A => Parser[String]): ParserOps[String] = ParserOps(f(a))

    case class ParserOps[A](p: Parser[A]) {
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
    def occurrences(regex: Regex): Parser[Int]
    def charOccurrences(c: Char): Parser[Int] = occurrences(s"$c*".r)
    def charOccurrencesAtStart(c: Char): Parser[Int] = occurrences(s"^$c*".r)
    val aOccs = occurrences("^a".r)

    /**
     * A Parser[Int] that recognizes one or more 'a' characters, and whose result
     * value is the number of 'a' characters it has seen. (Is this defined somehow in
     * terms of the same combinators as the parser for 'a' repeated zero or more
     * times?) The parser should fail when given a string without a starting 'a' . How
     * would you like to handle error reporting in this case? Could the API support giv-
     * ing an explicit message like "Expected one or more 'a'" in the case of failure?
     */
    def occurrencesOrError(regex: Regex): Parser[Int] = occurrences(regex).flatMap(n => if (n > 0) unit(n) else ???)

    /**
     * A parser that recognizes zero or more 'a' , followed by one or more 'b' , and
     * which results in the pair of counts of characters seen. For instance, given "bbb" ,
     * we get (0,3) , given "aaaab" , we get (4,1) , and so on.
     */
    def occurrenceList(regexList: List[Regex]): Parser[List[Int]]
    def charOccurrenceList(chList: List[Char]): Parser[List[Int]]

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
