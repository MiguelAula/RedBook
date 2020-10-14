import scala.annotation.tailrec
/*
package FPIScala_book {

  object Exercise_2_1 {
    /*
    Write a recursive function to get the nth Fibonacci number (http://mng.bz/C29s).
    The first two Fibonacci numbers are 0 and 1. The nth number is always the sum of the
    previous two—the sequence begins 0, 1, 1, 2, 3, 5. Your definition should use a
    local tail-recursive function.
    */
    def fib(n: Int): Int = {
      @tailrec def loop(n: Int, acc: Int = 0): Int = n match {
        case 0 => acc
        case x => loop(x - 1, acc + x)
      }

      loop(n)
    }

    def fib2(n: Int): Int = n match {
      case 0 => 0
      case x => x + fib2(x - 1)
    }

  }

  object Exercise_2_2 {
    /*
    Implement isSorted, which checks whether an Array[A] is sorted according to a
    given comparison function:
   */
    def isSorted[A](as: Array[A], ordered: (A, A) => Boolean): Boolean = {
      @tailrec
      def loop[A](a: Array[A], p: (A, A) => Boolean, n: Int): Boolean = {
        if (n+1 == a.length) true
        else if (!p(a(n), a(n + 1))) false
        else loop(a, p, n + 1)
      }

      loop(as, ordered, 0)
    }

    def main(args: Array[String]): Unit = {
      val sortedIntArr = Array(1,2,3,4,5)
      val unsortedIntArr = Array(1,5,2,4,3)
      val sortedCharArr = Array('a','b','c')
      val unsortedCharArr = Array('a','c','b')
      val intSortFunc = (a: Int, b: Int) => a < b
      val charSortFunc = (a: Char, b: Char) => a < b
      assert(isSorted(sortedIntArr,intSortFunc))
      assert(!isSorted(unsortedIntArr,intSortFunc))
      assert(isSorted(sortedCharArr,charSortFunc))
      assert(!isSorted(unsortedCharArr,charSortFunc))
    }
  }

  object exercise_2_3 {
    def curry[A,B,C](f: (A, B) => C): A => (B => C) = a => b => f(a,b)

    def uncurry[A,B,C](f: A => B => C): (A, B) => C = (a,b) => f(a)(b)

    def compose[A,B,C](f: B => C, g: A => B): A => C = a => f(g(a))
  }

  /**
   * Lists
   */
  object exercise_3 {

    sealed trait List[+A]
    case object Nil extends List[Nothing]
    case class Cons[+A](head: A, tail: List[A]) extends List[A]

    object List {
      def sum(ints: List[Int]): Int = ints match {
        case Nil => 0
        case Cons(x,xs) => x + sum(xs)
      }
      def product(ds: List[Double]): Double = ds match {
        case Nil => 1.0
        case Cons(0.0, _) => 0.0
        case Cons(x,xs) => x * product(xs)
      }
      def apply[A](as: A*): List[A] =
        if (as.isEmpty) Nil
        else Cons(as.head, apply(as.tail: _*))
      def tail[A](l: List[A]): List[A] = l match {
        case Nil =>  Nil
        case Cons(_,xs: List[A]) => xs
      }
      def setHead[A](l: List[A], head: A): List[A] = Cons(head,l)
      def append[A](a1: List[A], a2: List[A]): List[A] =
        a1 match {
          case Nil => a2
          case Cons(h,t) => Cons(h, append(t, a2))
        }

      @tailrec
      def make[A](item: A, n: Int, acc: List[A] = Nil: List[A]): List[A] = if (n == 0) acc else make(item,n-1,Cons(item,acc))

      @tailrec
      def drop[A](l: List[A], n: Int): List[A] = l match {
        case Nil => Nil
        case Cons(_,xs) => if (n > 0) drop(xs,n-1) else l
      }

      /**
       * Implement dropWhile , which removes elements from the List prefix as long as they
       * match a predicate.
       */
      @tailrec
      def dropWhile[A](l: List[A], f: A => Boolean): List[A] = l match {
        case Nil => Nil
        case Cons(x,xs) => if (f(x)) dropWhile(xs,f) else l
      }

      @tailrec
      def dropWhile2[A](as: List[A])(f: A => Boolean): List[A] =
        as match {
          case Cons(h,t) if f(h) => dropWhile2(t)(f)
          case _ => as
        }

      /**
       * Not everything works out so nicely. Implement a function, init , that returns a List
         *consisting of all but the last element of a List . So, given List(1,2,3,4) , init will
          *return List(1,2,3) . Why can’t this function be implemented in constant time like
          *tail ?
       */
      @tailrec
      def init[A](l: List[A]): List[A] = l match {
        case Nil => Nil
        case Cons(_,Nil) => l
        case Cons(_,xs) => init(xs)
      }

      def foldRight[A,B](as: List[A], z: B)(f: (A, B) => B): B =
        as match {
          case Nil => z
          case Cons(x, xs) => f(x, foldRight(xs, z)(f))
        }

      /**Can product , implemented using foldRight , immediately halt the recursion and
      return 0.0 if it encounters a 0.0 ? Why or why not? Consider how any short-circuiting
      might work if you call foldRight with a large list. This is a deeper question that we’ll
      return to in chapter 5.*/
      def foldRightShortCircuit[A,B](as: List[A], z: B)(f: (A, B) => B)(sf: A => Boolean): B = //le puedo poner default value a sf??
        as match {
          case Nil => z
          case Cons(x, xs) => if (!sf(x)) f(x, foldRightShortCircuit(xs, z)(f)(sf)) else z
        }

      def foldRightFL[A,B](as: List[A], z: B)(f: (A, B) => B): B =
        foldLeft(reverse(as),z)((x,xs) => f(xs,x))

      def foldLeftFR[A,B](as: List[A], z: B)(f: (B, A) => B): B =
        foldRight(as,z)((x,xs) => f(xs,x))

      /**Compute the length of a list using foldRight .*/
      def length[A](as: List[A]): Int = foldRight(as,0)((_,y) => 1 + y)

      @tailrec
      def foldLeft[A,B](as: List[A], z: B)(f: (B, A) => B): B =
        as match {
          case Nil => z
          case Cons(x,xs) => foldLeft(xs,f(z,x))(f)
        }

      def sumL(as: List[Int]): Int = foldLeft(as,0)(_ + _)
      def prodL(as: List[Int]): Int = foldLeft(as,1)(_ * _)

      def reverse[A](as: List[A]): List[A] = foldLeft(as,Nil:List[A])((xs,x) => Cons(x,xs))

      /** 3.14
       * Implement append in terms of either foldLeft or foldRight .
       */
      def appendFR[A](as: List[A], bs: List[A]): List[A] = foldRight(as,bs)(Cons(_,_))

      def appendFL[A](as: List[A], bs: List[A]): List[A] = foldLeft(reverse(as),bs)((xs,x) => Cons(x,xs))

      /** 3.23
       * Write a function that accepts two lists and constructs a new list by adding correspond-
        ing elements. For example, List(1,2,3) and List(4,5,6) become List(5,7,9)*/
      def zipWith[A,B](l1: List[A],l2: List[A])(f: (A,A) => B): List[B] = (l1,l2) match {
        case (Nil,Nil) => Nil
        case (Cons(x,xs),Cons(y,ys)) => Cons(f(x,y),zipWith(xs,ys)(f))
      }

      def flatten[A](l: List[List[A]]): List[A] =
        foldRight(l,Nil: List[A])((xs,x) => foldRight(xs,x)(Cons(_,_)))

      /** 3.16
       * Write a function that transforms a list of integers by adding 1 to each element.
        (Reminder: this should be a pure function that returns a new List !)*/
      def add1(l: List[Int]): List[Int] = foldRight(l,Nil:List[Int])((x,xs) => Cons(x+1,xs))

      /** 3.17
       * Write a function that turns each value in a List[Double] into a String . You can use
        the expression d.toString to convert some d: Double to a String .*/
      def doubleToStr(l: List[Double]): List[String] = foldRight(l,Nil:List[String])((x,xs) => Cons(x.toString,xs))

      /** 3.18
       * Write a function map that generalizes modifying each element in a list while maintain-
       * ing the structure of the list. Here is its signature:
       */
      def map[A,B](as: List[A])(f: A => B): List[B] = foldRight(as,Nil:List[B])((x,xs) => Cons(f(x),xs))

      /** 3.19
       * Write a function filter that removes elements from a list unless they satisfy a given
        predicate. Use it to remove all odd numbers from a List[Int] .*/
      def filter[A](as: List[A])(f: A => Boolean): List[A] = {
        @tailrec
        def loop[A](as: List[A], acc: List[A])(f: A => Boolean): List[A] =
          as match {
            case Nil => acc
            case Cons(x,xs) => if (f(x)) loop(xs,Cons(x,acc))(f) else loop(xs,acc)(f)
          }
        loop(reverse(as),List())(f)
      }

      /** 3.20
       * Write a function flatMap that works like map except that the function given will return
      a list instead of a single result, and that list should be inserted into the final resulting
      list.
      For instance, flatMap(List(1,2,3))(i => List(i,i)) should result in List(1,1,2,2,3,3) .
       */
      def flatMap[A,B](as: List[A])(f: A => List[B]): List[B] = flatten(map(as)(f))

      /** 3.21
       * Use flatMap to implement filter .
       * */
      def filterFM[A](as: List[A])(f: A => Boolean): List[A] = flatMap(as)(x => if (f(x)) List(x) else List())

      /** 3.22
       * Write a function that accepts two lists and constructs a new list by adding correspond-
       * ing elements. For example, List(1,2,3) and List(4,5,6) become List(5,7,9) .
       * */
      def zipAdd(as: List[Int],bs: List[Int]): List[Int] = zipWith(as,bs)(_ + _)

      //making this function to simplify exercise 3.24
      @tailrec
      def contains[A](as: List[A], elem: A): Boolean = as match {
        case Nil => false
        case Cons(x,_) if x == elem => true
        case Cons(x,xs) if x != elem => contains(xs,elem)
      }

      /** 3.24
       * Hard: As an example, implement hasSubsequence for checking whether a List con-
       * tains another List as a subsequence. For instance, List(1,2,3,4) would have
       * List(1,2) , List(2,3) , and List(4) as subsequences, among others. You may have
       * some difficulty finding a concise purely functional implementation that is also effi-
       * cient. That’s okay. Implement the function however comes most naturally. We’ll
       * return to this implementation in chapter 5 and hopefully improve on it. Note: Any
       * two values x and y can be compared for equality in Scala using the expression x == y .
       */
      def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean =
        length(flatMap(sup)(x => if (contains(sub,x)) List(x) else List())) == length(sub)
    }

    import FPIScala_book.exercise_3.List._
    def main(args: Array[String]): Unit = {
      val list: List[Int] = List(1,2,3,4,5,6,7)

      assert(dropWhile(list,(x: Int) => x <= 3) == List(4,5,6,7))
      assert(drop(list,6) == List(7))
      assert(init(list) == List(7))
      assert(dropWhile2(list)(_ <= 4) == List(5,6,7))
      assert(foldRightShortCircuit(list,1)(_ * _)(_ == 3) == 2)
      assert(foldRight(List(1,2,3), Nil:List[Int])(Cons(_,_)) == Cons(1,Cons(2,Cons(3,Nil))))
      assert(length(list) == 7)
      assert(reverse(list) == List(7,6,5,4,3,2,1))
      assert(foldRightFL(List(1,2,3), Nil:List[Int])(Cons(_,_)) == Cons(1,Cons(2,Cons(3,Nil))))
      assert(appendFR(List(1,2),List(3,4)) == List(1,2,3,4))
      assert(appendFL(List(1,2),List(3,4)) == List(1,2,3,4))
      assert(zipWith(List(1,2),List(3,4))(_.toString + _.toString) == List("13","24"))
      assert(flatten(List(List(1,2),List(3),List(4,5,6))) == List(1,2,3,4,5,6))
      assert(add1(List(1,2,3)) == List(2,3,4))
      assert(doubleToStr(List(1.0,2.0,3.0)) == List("1.0","2.0","3.0"))
      assert(map(List(1,2,3))(_.toString+"hehe") == List("1hehe","2hehe","3hehe"))

      val longList = make(1,10000)

      //foldRight(longList,0)(_ + _) //STACK OVERFLOW!
      assert(foldRightFL(longList,0)(_ + _) == 10000)
      assert(filter(List(1,2,3,4,5,6))(_ % 2 != 0) == List(1,3,5))
      assert(flatMap(List(1,2,3))(i => List(i,i)) == List(1,1,2,2,3,3))
      assert(filter(List(1,2,3,4,5,6))(_ % 2 != 0) == filterFM(List(1,2,3,4,5,6))(_ % 2 != 0))
      assert(zipAdd(List(1,2,3),List(4,5,6)) == List(5,7,9))

      assert(hasSubsequence(List(1,2,3,4),List(2,4)))
      assert(!hasSubsequence(List(1,2,3,4),List(2,3,5)))
    }

  }

  /**
   * Trees
   */
  object Exercise_3_5 {
    sealed trait Tree[+A]
    case class Leaf[A](value: A) extends Tree[A]
    case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

    /**
     * Write a function size that counts the number of nodes (leaves and branches) in a tree.
     */
    def size[A](t: Tree[A]): Int = t match {
      case Leaf(_) => 1
      case Branch(l,r) => size(l) + size(r) + 1
    }

    /**
     * Write a function maximum that returns the maximum element in a Tree[Int] . (Note:
     * In Scala, you can use x.max(y) or x max y to compute the maximum of two integers x
     * and y .)
     */
    def maximum(t: Tree[Int]): Int = t match {
      case Leaf(x) => x
      case Branch(l,r) => maximum(l) max maximum(r)
    }

    /** 3.27
     * Write a function depth that returns the maximum path length from the root of a tree
     * to any leaf.
     */
    def depth[A](t: Tree[A]): Int = t match {
      case Leaf(_) => 1
      case Branch(l,r) => (depth(l) max depth(r)) + 1
    }

    /** 3.28
     * Write a function map , analogous to the method of the same name on List , that modi-
     * fies each element in a tree with a given function.
     */
    def map[A,B](t: Tree[A])(f: A => B): Tree[B] = t match {
      case Leaf(x) => Leaf(f(x))
      case Branch(l,r) => Branch(map(l)(f),map(r)(f))
    }

    /** 3.29
     * Generalize size , maximum , depth , and map , writing a new function fold that abstracts
     * over their similarities. Reimplement them in terms of this more general function. Can
     * you draw an analogy between this fold function and the left and right folds for List ?
     * */
    def fold[A,B](t: Tree[A], z: A => B)(f: (B,B) => B): B = t match {
      case Leaf(x) => z(x)
      case Branch(l,r) => f(fold(l,z)(f),fold(r,z)(f))
    }

    def maximumF(t: Tree[Int]): Int = fold(t,(x: Int) => x)((t1,t2) => t1 max t2)
    def depthF[A](t: Tree[A]): Int = fold(t,(_: A) => 1)((t1,t2) => (t1 max t2) + 1)
    def mapF[A,B](t: Tree[A])(f: A => B): Tree[B] = fold(t,((x: A) => Leaf(f(x))): A => Tree[B])((t1,t2) => Branch(t1,t2))

    def main(args: Array[String]): Unit = {
      val tree: Tree[Int] = Branch(Branch(Leaf(1),Leaf(2)),Branch(Leaf(3),Branch(Leaf(5),Leaf(6))))
      assert(size(tree) == 9)
      assert(maximum(tree) == 6)
      assert(depth(tree) == 4)

      val treePlus1 = map(tree)(_ + 1)
      assert(maximum(treePlus1) == 7)

      assert(maximumF(tree) == 6)
      assert(depthF(tree) == 4)
      assert(maximumF(treePlus1) == 7)
    }
  }

  /**
   *  Option
   */
  object Exercise_4_option {
    sealed trait Option[+A] {
      def map[B](f: A => B): Option[B] = this match {
        case None => None
        case Some(get) => Some(f(get))
      }
      def flatMap[B](f: A => Option[B]): Option[B] = this match {
        case None => None
        case Some(get) => f(get)
      }
      def getOrElse[B >: A](default: => B): B = this match {
        case None => default
        case Some(get) => get
      }
      def orElse[B >: A](ob: => Option[B]): Option[B] = this match {
        case None => ob
        case Some(_) => this
      }
      def filter(f: A => Boolean): Option[A] = this match {
        case None => this
        case Some(x) => if (f(x)) this else None
      }
    }
    case class Some[+A](get: A) extends Option[A]
    case object None extends Option[Nothing]

    /** Employees */
    case class Employee(name: String, department: String)
    val employees: List[Employee] = List(Employee("Joe","I+D"),Employee("Kevin","Informatics"),Employee("Mary","HHRR"))
    def lookupByName(name: String): Option[Employee] = None
    val joeDepartment: Option[String] =
      lookupByName("Joe").map(_.department)

    //we implement mean to simplify variance implementation
    def mean(xs: Seq[Double]): Option[Double] = if (xs.isEmpty) None else Some(xs.sum / xs.length)

    /**Implement the variance function in terms of flatMap . If the mean of a sequence is m ,
    the variance is the mean of math.pow(x - m, 2) for each element x in the sequence.
    See the definition of variance on Wikipedia (http://mng.bz/0Qsr).*/
    def variance(xs: Seq[Double]): Option[Double] = {
      mean(xs).flatMap(m => mean(xs.map(dbl => math.pow(dbl - m,2))))
    }

    def Try[A](a: => A): Option[A] =
      try Some(a)
      catch { case e: Exception => None }

    /**
    Write a generic function map2 that combines two Option values using a binary func-
      tion. If either Option value is None , then the return value is too. Here is its signature:
     */
    def map2[A,B,C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] = (a,b) match {
      case (None,_) | (_,None) => None
      case (Some(a),Some(b)) => Some(f(a,b))
    }

    def map2bookVersion[A,B,C](a: Option[A], b: Option[B])(f: (A, B) => C):
    Option[C] =
      a flatMap (aa =>
        b map (bb =>
          f(aa, bb)))

    /** 4.4
     * Write a function sequence that combines a list of Options into one Option containing
     * a list of all the Some values in the original list. If the original list contains None even
     * once, the result of the function should be None ; otherwise the result should be Some
     * with a list of all the values. Here is its signature:
     */
    def sequence[A](a: List[Option[A]]): Option[List[A]] =
      Try(a.map(x => x.getOrElse(throw new Exception())))
      //traverse(a)(x => x)

    /** 4.5
     * Implement the traverse function. It’s straightforward to do using map and sequence , but try
     * for a more efficient implementation that only looks at the list once. In fact, imple-
     * ment sequence in terms of traverse .
     */
    def traverse[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] = Try(a.map(x => f(x).getOrElse(throw new Exception())))

    def sequenceT[A](a: List[Option[A]]): Option[List[A]] = traverse(a)(x => x)

    def main(args: Array[String]): Unit = {
      println(variance(List(1,2,3)).getOrElse("Variance of empty list"))

      val listOk = List("1","2","3","4").map(x => Try(x.toInt))
      val listNotOk = List("1","2","tres","4").map(x => Try(x.toInt))
      assert(sequence(listOk).getOrElse(List()) == List(1,2,3,4))
      assert(sequence(listNotOk) == None)
      assert(sequenceT(listNotOk) == sequence(listNotOk) && sequenceT(listOk) == sequence(listOk))

      assert(map2(Some(1),Some(2))((a,b) => a+b) == map2bookVersion(Some(1),Some(2))((a,b) => a+b))
      assert(map2(Some(1),None)((x,y: Nothing) => (x,y)) == map2bookVersion(Some(1),None)((x,y: Nothing) => (x,y)))
    }
  }


  object Exercise_4_either {
    sealed trait Either[+E, +A] {
      def map[B](f: A => B): Either[E, B] = this match {
        case Right(v) => Try(f(v))
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
        case (Right(v),Right(v2)) => Try(f(v,v2))
      }
    }
    case class Left[+E](value: E) extends Either[E, Nothing]
    case class Right[+A](value: A) extends Either[Nothing, A]

    def Try[A,E](a: => A): Either[E, A] =
      try Right(a)
      catch { case e: E => Left(e) }

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

    def main(args: Array[String]): Unit = {}
  }

  object Exercise_5_streaming {

    sealed trait Stream[+A] {
      /** Selfmade map... is there a better way to do this without adding () => every time? */
      def map[B](f: A => B): Stream[B] = this match {
        case Empty => Empty
        case Cons(h,t) => Cons(() => f(h()),() => t().map(f))
      }

      @tailrec
      final def foldLeft[B](z: B)(f: (B,A) => B): B = this match {
        case Empty => z
        case Cons(h,t) => t().foldLeft(f(z,h()))(f)
      }

      def reverse: Stream[A] = this.foldLeft(Empty: Stream[A])((b,a) => Cons(() => a,() => b))
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
            case Cons(h,t) => loop(t(),h() :: acc)
          }
        loop(this,List())
      }

      /** 5.2
       * Write the function take(n) for returning the first n elements of a Stream , and
       * drop(n) for skipping the first n elements of a Stream .
       */
      def take(n: Int): Stream[A] = if (n == 0) Empty else this match {
        case Empty => throw new Exception(s"Reached end of stream with $n more elements to take")
        case Cons(h,tail) => Cons(h,() => tail().take(n-1))
      }

      /**
       * tailrec version of take(n)
       */
      def take2(n: Int): Stream[A] = {
        @tailrec
        def loop(s: Stream[A], n: Int, acc: Stream[A]): Stream[A] =
          if (n == 0) acc.reverse else s match {
            case Empty => throw new Exception (s"Reached end of stream with $n more elements to take")
            case Cons(h,tail) => loop(tail(),n-1, Cons(h,() => acc))
          }
        loop(this,n,Empty)
      }

      def drop(n: Int): Stream[A] = if (n == 0) this else this match {
        case Empty => throw new Exception(s"Reached end of stream with $n more elements to drop")
        case Cons(_,tail) => tail().drop(n-1)
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
            case Cons(h,_) if !p(h()) => acc.reverse
            case Cons(h,tail) if p(h()) => loop(tail(),Cons(h,() => acc))
          }
        loop(this,Empty)
      }

      def foldRight[B](z: => B)(f: (A, => B) => B): B =
        this match {
          case Cons(h,t) => f(h(), t().foldRight(z)(f))
          case _ => z
        }

      /** 5.4
       * Implement forAll , which checks that all elements in the Stream match a given predi-
       * cate. Your implementation should terminate the traversal as soon as it encounters a
       * nonmatching value.
       */
      def forAll(p: A => Boolean): Boolean = this match {
        case Empty => true
        case Cons(x,xs) => if (p(x())) xs().forAll(p) else false
      }

      /** 5.5
       * Use foldRight to implement takeWhile .
       * */
      def takeWhileFoldRight(p: A => Boolean): Stream[A] = {
        this.foldRight(Empty: Stream[A])((x,zs) => if (p(x)) Cons(() => x,() => zs) else zs )
      }

      /** (taken from book)
       * Here’s a function to optionally extract the head of
       * a Stream
       */
      def headOption: Option[A] = this match {
        case Empty => None
        case Cons(h, t) => Some(h())
      }

      /** 5.6
       * Hard: Implement headOption using foldRight .
       */
      def headOptionFoldRight: Option[A] =
        this.foldRight(None: Option[A])((x,zs) =>
          zs match {
            case None => Some(x)
            case Some(_) => Some(x)
          })

      def headOptionFoldLeft: Option[A] =
        this.foldLeft(None: Option[A])((zs,x) =>
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
            case Empty => acc
            case Cons(x,xs) => loop(xs(),Cons[B](() => f(x()),() => acc))
          }
        loop(this,Empty)
      }
      def mapFoldRight[B](f: A => B): Stream[B] = this.foldRight(Empty[B])((x,zs) => Cons(() => f(x),() => zs))

      def filter(f: A => Boolean): Stream[A] = {
        def loop(s: Stream[A], acc: Stream[A]): Stream[A] =
          s match {
            case Empty => acc
            case Cons(x,xs) if f(x()) => loop(xs(),Cons(x,() => acc))
            case Cons(x,xs) if !f(x()) => loop(xs(),acc)
          }
        loop(this,Empty)
      }
      def filterFoldRight(f: A => Boolean): Stream[A] = this.foldRight(Empty[A])((x,zs) => if (f(x)) Cons(() => x,() => zs) else zs)

      def append[B >: A](elem: => B): Stream[B] = Cons(() => elem,() => this)

      def concat[B >: A](that: Stream[B]): Stream[B] = that match {
        case Empty => this
        case Cons(x,xs) => this.append(x).concat(xs())
      }

      def flatMap[B](f: A => Stream[B]): Stream[B] = {
        def loop(s: Stream[A], acc: Stream[B]): Stream[B] = {
          s match {
            case Empty => acc
            case Cons(x,xs) =>
              loop(xs(),f(x()).concat(xs()))
          }
        }
        loop(this,Empty)
      }
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
    }

    def main(args: Array[String]): Unit = {
      val s = Stream(1,2,3,4,5)
      assert(s.toList == List(1,2,3,4,5))
      assert(s.map(_ + 1).toList == List(2,3,4,5,6))

      assert(s.take(2).toList == List(1,2))
      assert(s.take2(2).toList == List(1,2))
      assert(s.takeWhile(_ < 3).toList == List(1,2))
      assert(s.forAll(x => x < 6))
      assert(!s.forAll(x => x != 3))

      assert(s.takeWhileFoldRight(x => x < 3 || x > 4).toList == List(1,2,5))

      assert(s.headOption == s.headOptionFoldRight && s.headOption == s.headOptionFoldLeft)
      assert(Stream().headOption == Stream().headOptionFoldRight && Stream().headOption == Stream().headOptionFoldLeft)

      println(s.map(x => x+1).toList)
    }
  }

}
*/