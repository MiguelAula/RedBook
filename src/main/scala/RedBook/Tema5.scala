package RedBook

import scala.annotation.tailrec

object Exercise_5_streaming {

  sealed trait Stream[+A] {
    @tailrec
    final def foldLeft[B](z: B)(f: (B, A) => B): B = this match {
      case Empty => z
      case Cons(h, t) => t().foldLeft(f(z, h()))(f)
    }

    def reverse: Stream[A] = this.foldLeft(Empty: Stream[A])((b, a) => Cons(() => a, () => b))

    def exists(p: A => Boolean): Boolean = this match {
      case Cons(h, t) => p(h()) || t().exists(p)
      case _ => false
    }

    /** (taken from book)
     * Here’s a function to optionally extract the head of
     * a Stream
     */
    def headOption: Option[A] = this match {
      case Empty => None
      case Cons(h, t) => Some(h())
    }

    /** selfmade */
    def tail: Stream[A] = this match {
      case Empty => Empty
      case Cons(_, t) => t()
    }

    /** 5.1
     * Write a function to convert a Stream to a List , which will force its evaluation and let
     * you look at it in the REPL . You can convert to the regular List type in the standard
     * library. You can place this and other functions that operate on a Stream inside the
     * Stream trait.
     */
    def toList: List[A] = {
      @tailrec
      def loop(l: Stream[A], acc: List[A]): List[A] =
        l match {
          case Empty => acc.reverse
          case Cons(h, t) => loop(t(), h() :: acc)
        }

      loop(this, List())
    }

    /** 5.2
     * Write the function take(n) for returning the first n elements of a Stream , and
     * drop(n) for skipping the first n elements of a Stream .
     */
    def take(n: Int): Stream[A] = if (n == 0) Empty else this match {
      case Empty => throw new Exception(s"Reached end of stream with $n more elements to take")
      case Cons(h, tail) => Cons(h, () => tail().take(n - 1))
    }

    /**
     * tailrec version of take(n)
     */
    def take2(n: Int): Stream[A] = {
      @tailrec
      def loop(s: Stream[A], n: Int, acc: Stream[A]): Stream[A] =
        if (n == 0) acc.reverse else s match {
          case Empty => throw new Exception(s"Reached end of stream with $n more elements to take")
          case Cons(h, tail) => loop(tail(), n - 1, Cons(h, () => acc))
        }

      loop(this, n, Empty)
    }

    def drop(n: Int): Stream[A] = if (n == 0) this else this match {
      case Empty => throw new Exception(s"Reached end of stream with $n more elements to drop")
      case Cons(_, tail) => tail().drop(n - 1)
    }

    /** 5.3
     * Write the function takeWhile for returning all starting elements of a Stream that
     * match the given predicate.
     */
    def takeWhile(p: A => Boolean): Stream[A] = {
      @tailrec
      def loop(s: Stream[A], acc: Stream[A]): Stream[A] =
        s match {
          case Empty => acc.reverse
          case Cons(h, _) if !p(h()) => acc.reverse
          case Cons(h, tail) if p(h()) => loop(tail(), Cons(h, () => acc))
        }

      loop(this, Empty)
    }

    def foldRight[B](z: => B)(f: (A, => B) => B): B =
      this match {
        case Cons(h, t) => f(h(), t().foldRight(z)(f))
        case _ => z
      }

    /** 5.4
     * Implement forAll , which checks that all elements in the Stream match a given predi-
     * cate. Your implementation should terminate the traversal as soon as it encounters a
     * nonmatching value.
     */
    def forAll(p: A => Boolean): Boolean = this match {
      case Empty => true
      case Cons(x, xs) => if (p(x())) xs().forAll(p) else false
    }

    /** 5.5
     * Use foldRight to implement takeWhile .
     * */
    def takeWhileFoldRight(p: A => Boolean): Stream[A] = {
      this.foldRight(Empty: Stream[A])((x, zs) => if (p(x)) Cons(() => x, () => zs) else zs)
    }

    /** 5.6
     * Hard: Implement headOption using foldRight .
     */
    def headOptionFoldRight: Option[A] =
      this.foldRight(None: Option[A])((x, zs) =>
        zs match {
          case None => Some(x)
          case Some(_) => Some(x)
        })

    def headOptionFoldLeft: Option[A] =
      this.foldLeft(None: Option[A])((zs, x) =>
        zs match {
          case None => Some(x)
          case Some(a) => Some(a)
        })

    /** 5.7
     * Implement map , filter , append , and flatMap using foldRight . The append method
     * should be non-strict in its argument.
     */
    def map[B](f: A => B): Stream[B] = {
      def loop(s: Stream[A], acc: Stream[B]): Stream[B] =
        s match {
          case Empty => acc.reverse
          case Cons(x, xs) => loop(xs(), Cons[B](() => f(x()), () => acc))
        }

      loop(this, Empty)
    }

    def mapFoldRight[B](f: A => B): Stream[B] = this.foldRight(Empty: Stream[B])((x, zs) => Cons(() => f(x), () => zs))

    def filter(f: A => Boolean): Stream[A] = {
      def loop(s: Stream[A], acc: Stream[A]): Stream[A] =
        s match {
          case Empty => acc.reverse
          case Cons(x, xs) if f(x()) => loop(xs(), Cons(x, () => acc))
          case Cons(x, xs) if !f(x()) => loop(xs(), acc)
        }

      loop(this, Empty)
    }

    def filterFoldRight(f: A => Boolean): Stream[A] = this.foldRight(Empty: Stream[A])((x, zs) => if (f(x)) Cons(() => x, () => zs) else zs)

    def append[B >: A](elem: => B): Stream[B] =
      this.foldRight(Empty: Stream[B])((x, zs) => {
        val tail = zs match {
          case Empty => Cons(() => elem, () => Empty)
          case Cons(a, b) => Cons(a, b)
        }
        Cons(() => x, () => tail)
      }
      )

    /**
     * ???? Aquesta era la meva def original de ::: . Aquesta def inverteix l'ordre de les 2 llistes però no veig el motiu
     */
    //def :::[B >: A] (that: Stream[B]): Stream[B] = this.foldRight(that)((x,zs) => Cons(() => x,() => zs))

    def :::[B >: A](that: Stream[B]): Stream[B] = that.foldRight(this: Stream[B])((x, zs) => Cons(() => x, () => zs))

    /**
     * ???? COM FAIG UNA CORRECTA DEFINICIÓ DE FLATTEN? Voldria dir-li que A es realment Stream[B]
     */
    //def flatten[B]: Stream[B] = this.foldRight(Empty: Stream[B])((xs,zs) => xs ::: zs)

    def flatMap[B](f: A => Stream[B]): Stream[B] =
      this.map(f).foldRight(Empty: Stream[B])((xs, zs) => xs ::: zs)

    /**
     * EXERCISE 5.13
     * Use unfold to implement (map , take , takeWhile ,) zipWith (as in chapter 3), and
     * zipAll . The zipAll function should continue the traversal as long as either stream
     * has more elements—it uses Option to indicate whether each stream has been
     * exhausted.
     */
    def zipWith[B, C](s2: Stream[B])(f: (A, B) => C): Stream[C] =
      Stream.unfold((this, s2)) {
        case (s1, s2) =>
          val x1 = s1.headOption
          val x2 = s2.headOption
          (x1, x2) match {
            case (None, None) => None
            case (x, y) => Some(f(x.get, y.get), (s1.tail, s2.tail))
          }
      }

    def zipAll[B](s2: Stream[B]): Stream[(Option[A], Option[B])] =
      Stream.unfold((this, s2)) {
        case (s1, s2) =>
          val x1 = s1.headOption
          val x2 = s2.headOption
          if (x1.isEmpty && x2.isEmpty) None else Some((x1, x2), (s1.tail, s2.tail))
      }

    /**
     * EXERCISE 5.14
     * Hard: Implement startsWith using functions you’ve written. It should check if one
     * Stream is a prefix of another. For instance, Stream(1,2,3) startsWith Stream(1,2)
     * would be true . */
    def startsWith[B](s: Stream[B]): Boolean = {
      val l = s.toList
      this.take(l.length).toList == l
    }

    /** ???? better way to do this without .append? maybe start with   this :: Stream.unfold(...) would be better?
     * EXERCISE 5.15
     * Implement tails using unfold . For a given Stream , tails returns the Stream of suf-
     * fixes of the input sequence, starting with the original Stream . For example, given
     * Stream(1,2,3) , it would return Stream(Stream(1,2,3), Stream(2,3), Stream(3), Stream()) .
     */
    def tails: Stream[Stream[A]] =
      Stream.unfold(this) {
        case Empty => None
        case s@Cons(_, tail) => Some(s, tail())
      }.append(Stream())

    /** ???? no usa intermediate results D: mejorable
     * EXERCISE 5.16
     * Hard: Generalize tails to the function scanRight , which is like a foldRight that
     * returns a stream of the intermediate results. For example:
     * scala> Stream(1,2,3).scanRight(0)(_ + _).toList
     * res0: List[Int] = List(6,5,3,0)
     * This example should be equivalent to the expression List(1+2+3+0, 2+3+0, 3+0,0) .
     * Your function should reuse intermediate results so that traversing a Stream with n
     * elements always takes time linear in n . Can it be implemented using unfold ? How, or
     * why not? Could it be implemented using another function we’ve written?
     */
    def scanRight[B](z: B)(f: (A, => B) => B): Stream[B] =
      Stream.unfold(this) {
        case Empty => None
        case s@Cons(x, tail) =>
          val next = s.foldRight(z)(f)
          Some(next, tail())
      }.append(z)
  }

  case object Empty extends Stream[Nothing]
  case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]
  object Stream {
    def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
      lazy val head = hd
      lazy val tail = tl
      Cons(() => head, () => tail)
    }
    def empty[A]: Stream[A] = Empty
    def apply[A](as: A*): Stream[A] = {
      if (as.isEmpty) empty else cons(as.head, apply(as.tail: _*))
    }

    /** ------------------------------------------------------ */
    /**
     * EXERCISE 5.8
     * Generalize ones slightly to the function constant , which returns an infinite Stream of
     * a given value.
     */
    def constant[A](a: A): Stream[A] = cons(a,constant(a))

    /**
     * EXERCISE 5.9
     * Write a function that generates an infinite stream of integers, starting from n , then n
     * + 1 , n + 2 , and so on.
     */
    def from(n: Int): Stream[Int] = cons(n,from(n+1))

    /**
     * EXERCISE 5.10
     * Write a function fibs that generates the infinite stream of Fibonacci numbers: 0, 1, 1,
     * 2, 3, 5, 8, and so on.
     */
    def fibs: Stream[Int] = {
      def loop(acc1: Int, acc2: Int): Stream[Int] = cons(acc1,loop(acc2,acc1+acc2))
      loop(0,1)
    }

    /**
     * EXERCISE 5.11
     * Write a more general stream-building function called unfold . It takes an initial state,
     * and a function for producing both the next state and the next value in the generated
     * stream.
     */
    def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = f(z) match {
      case Some((head,tail)) => cons(head,unfold(tail)(f))
      case None => Empty
    }

    /**
     * EXERCISE 5.12
     * Write fibs , from , constant , and ones in terms of unfold .
     */
    def constantUnfold[A](a: A): Stream[A] = unfold(a)(x => Some((x,x)))
    def fromUnfold(n: Int): Stream[Int] = unfold(n)(x => Some((x,x+1)))
    def fibsUnfold: Stream[Int] = unfold((0,1)){case (x1,x2) => Some(x1,(x2,x1+x2))}
  }

  def main(args: Array[String]): Unit = {
    val s = Stream(1,2,3,4,5)
    assert(s.toList == List(1,2,3,4,5))

    assert(s.take(2).toList == List(1,2))
    assert(s.take2(2).toList == List(1,2))
    assert(s.takeWhile(_ < 3).toList == List(1,2))
    assert(s.forAll(x => x < 6))
    assert(!s.forAll(x => x != 3))

    assert(s.takeWhileFoldRight(x => x < 3 || x > 4).toList == List(1,2,5))

    assert(s.headOption == s.headOptionFoldRight && s.headOption == s.headOptionFoldLeft)
    assert(Stream().headOption == Stream().headOptionFoldRight && Stream().headOption == Stream().headOptionFoldLeft)

    assert(s.map(_ + 1).toList == List(2,3,4,5,6))
    assert(s.mapFoldRight(_ + 1).toList == s.map(_ + 1).toList)

    assert(s.filter(_ < 5).toList == List(1,2,3,4))
    assert(s.filter(_ < 5).toList == s.filterFoldRight(_ < 5).toList)

    assert(s.append(6).toList == List(1,2,3,4,5,6))

    assert((Stream(1,2,3) ::: Stream("a","b","c")).toList == List(1,2,3,"a","b","c"))
    assert(s.flatMap(x => Stream(x,x)).toList == List(1, 1, 2, 2, 3, 3, 4, 4, 5, 5))

    /**
     * ???? el redbook me dice que defina este "ones" (p.94) pero el compilador peta por forward reference
     */
    /*
    val ones: Stream[Int] = Stream.cons(1, ones)
    println(ones.map(_ + 1).exists(_ % 2 == 0))
    println(ones.takeWhile(_ == 1))
    println(ones.forAll(_ != 1))
    */

    assert(Stream.fibs.take(7).toList == List(0,1,1,2,3,5,8))
    assert(Stream.constant(3).take(5).toList == List(3,3,3,3,3))
    assert(Stream.from(5).take(3).toList == List(5,6,7))

    assert(Stream.constant(3).take(5).toList == Stream.constantUnfold(3).take(5).toList)
    assert(Stream.from(5).take(3).toList == Stream.fromUnfold(5).take(3).toList)
    assert(Stream.fibs.take(7).toList == Stream.fibsUnfold.take(7).toList)

    assert(Stream(1,2,3).zipAll(Stream(1,2,3,4)).toList == List((Some(1),Some(1)),(Some(2),Some(2)),(Some(3),Some(3)),(None,Some(4))))

    assert(Stream(1,2,3) startsWith Stream(1,2))

    assert(Stream(1,2,3).tails.map(_.toList).toList == Stream(Stream(1,2,3),Stream(2,3),Stream(3),Stream()).map(_.toList).toList)

    assert(Stream(1,2,3).scanRight(0)(_ + _).toList == List(6, 5, 3, 0))
  }
}