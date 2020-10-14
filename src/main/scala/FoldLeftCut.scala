object FoldLeftCut {

  class MyList {
    def foldLeft[A, B](xs: Seq[A], z: B)(op: (B, A) => B): B = {
      def f(xs: Seq[A], acc: B): B = xs match {
        case Seq()   => acc
        case x +: xs => f(xs, op(acc, x))
      }
      f(xs, z)
    }
  }

}
