package RedBook

import scala.annotation.tailrec

object Exercise_4_either {
  sealed trait Either[+E, +A] {
    def map[B](f: A => B): Either[E, B] = this match {
      case Right(v) => Right(f(v))
      case Left(v) => Left(v)
    }
    def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] = this match {
      case Right(v) => f(v)
      case Left(v) => Left(v)
    }
    def orElse[EE >: E,B >: A](b: => Either[EE, B]): Either[EE, B] = this match {
      case Right(_) => this
      case _ => b
    }
    def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] = (this,b) match {
      case (Left(e),_) => Left(e)
      case (_,Left(e)) => Left(e)
      case (Right(v),Right(v2)) => Right(f(v,v2))
    }
  }
  case class Left[+E](value: E) extends Either[E, Nothing]
  case class Right[+A](value: A) extends Either[Nothing, A]

  /*
  def Try[A](a: => A): Either[Exception, A] =
    try Right(a)
    catch { case e: Exception => Left(e) }
  */
  /**
   * ????
   * El redbook me daba el código de arriba (p.82) pero el compilador se queja por los tipos! He tenido que usar este de abajo, pero este da un warning...
   */

  def Try[A,B <: Exception](a: => A): Either[B, A] =
    try Right(a)
    catch { case e: B => Left(e) }



  /**Implement sequence and traverse for Either . These should return the first error
    that’s encountered, if there is one.*/
  def sequence[E, A](es: List[Either[E, A]]): Either[E, List[A]] = {
    @tailrec
    def loop(es: List[Either[E, A]], acc: Either[E, List[A]]): Either[E, List[A]] = es match {
      case Nil => acc
      case x :: xs => x match {
        case Left(e) => Left(e)
        case Right(v) => loop(xs, acc.map(l => v :: l))
      }
    }

    loop(es, Right(List()))
  }
  //traverse(es)(x => x)

  def traverse[E, A, B](as: List[A])(f: A => Either[E, B]): Either[E, List[B]] = {
    @tailrec
    def loop(es: List[A], acc: Either[E, List[B]]): Either[E, List[B]] = es match {
      case Nil => acc
      case x :: xs => f(x) match {
        case Left(e) => Left(e)
        case Right(v) => loop(xs, acc.map(l => v :: l))
      }
    }

    loop(as, Right(List()))
  }

  def main(args: Array[String]): Unit = {
    val a: Either[String,Int] = Right(1)
    val b: Either[String, Int] = a.map(i => i/0)
    b match {
      case Left(e) => println(e.getClass.getName)
      case Right(v) => println(v.getClass.getName)
    }

  }
}