package RedBook

import scala.annotation.tailrec


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
