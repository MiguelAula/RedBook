import scala.annotation.tailrec

object testerino extends App{
  val as = List(1,2,3)

  val b = as match {
      case l: List[Boolean] => false
      case l: List[Int] => true
      case l => false
    }

  println(b)

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
}
