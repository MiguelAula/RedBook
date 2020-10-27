package RedBook

import scala.annotation.tailrec


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
    def foldRightShortCircuit[A,B](as: List[A], z: B)(f: (A, B) => B)(sf: A => Boolean): B =
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

  import exercise_3.List._
  def main(args: Array[String]): Unit = {
    val list = List(1,2,3,4,5,6,7)

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