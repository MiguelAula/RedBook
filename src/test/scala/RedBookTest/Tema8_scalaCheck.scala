package RedBookTest

import org.scalacheck.Prop.forAll
import org.scalacheck.{Gen, Properties}

/**
 * EXERCISE 8.1
 * To get used to thinking about testing in this way, come up with properties that specify
 * the implementation of a sum: List[Int] => Int function. You don’t have to write your
 * properties down as executable ScalaCheck code—an informal description is fine.
 * Here are some ideas to get you started:
 * Reversing a list and summing it should give the same result as summing the
 * original, nonreversed list.
 * What should the sum be if all elements of the list are the same value?
 * Can you think of other properties?
 */
object SumListInt extends Properties("SumListInt") {

  property("reverseSum") = forAll { (l: List[Int]) =>
    l.sum == l.reverse.sum
  }

  val tinyInt: Gen[Int] = Gen.choose(0,500)
  property("sameIntList") = forAll(tinyInt,tinyInt) { (n,int) =>
    List.fill(n)(int).sum == n*int
  }
}

/**
 * EXERCISE 8.2
 * What properties specify a function that finds the maximum of a List[Int] ?
 */
object MaxListInt extends Properties("MaxListInt") {

  property("reverseMax") = forAll { (l: List[Int]) =>
    l.maxOption == l.reverse.maxOption
  }

  property("sameIntList") = forAll { (l: List[Int], a: Int) =>
    val oldMax = l.maxOption
    val newList = a :: l
    if (a >= oldMax.getOrElse(Int.MinValue)) newList.max == a
    else newList.maxOption == oldMax
  }
}