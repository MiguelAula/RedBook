import RedBook.PropImplementation.Gen
import RedBook.functional_RNG.SimpleRNG
import com.sun.tools.javac.code.TypeTag

import scala.annotation.{nowarn, tailrec}
import scala.collection.View

object testerino extends App{
  val as = List(1,2,3)
  @nowarn
  def f[A](l: List[A]): Boolean =
    l match {
      case l: List[Boolean] => false
      case l: List[Int] => true
      case l => false
    }

  println(f(as))

  //---------------------------------------------------------

  @tailrec
  def foldLeftCut[A,B](l: List[A])(z: B)(f: (A,B) => B, cond: B => Boolean): B = {
    l match {
      case x :: xs =>
        val next = f(x,z)
        if (cond(next)) foldLeftCut(xs)(next)(f,cond)
        else z
      case Nil => z
    }
  }

  println(foldLeftCut(List(1,2,3,4,5,6))(0)(_ + _,_ <= 11))

  //------------------------------------------------------------
  /*
  case class Potato(temp: Int) { self =>
    def run: String = temp match {
      case _ < 15 => "cold potato"
      case _ >= 15 & _ < 30 => "mild potato"
      case _ => "hot potato"
    }
    implicit def setTemp(implicit t: Int): Potato = new Potato(t)
  }

  val ambientTemp = 20
  val P: Potato = Potato(ambientTemp)
  */

  def bytesStream: LazyList[Byte] = LazyList.range(Byte.MinValue,Byte.MaxValue)

  def listRange(min: Int, max: Int): LazyList[Int] = if (min <= max) min #:: listRange(min+1,max) else LazyList.empty

  val lens = bytesStream.toList
  println(lens)

  val b = listRange(-10,10).toList
  println(b)

  val arr = LazyList('a','b','c','d','e','f','g','h')
  def popRandom(arr: LazyList[Char], arrLength: Int): (Char,LazyList[Char]) = {
    val randomInt = Gen.choose(0,arrLength).sample.run(SimpleRNG(System.currentTimeMillis()))._1
    (arr(randomInt),arr.slice(0,randomInt) ++ arr.slice(randomInt+1,arrLength))
  }
  val popRes = popRandom(arr,8)
  println(arr)
  println(popRes._1 + " | " + popRes._2)
}
