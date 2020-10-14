object MyModule {
  def factorial(n: Int): Int = {
    def go(n: Int, acc: Int = 1): Int =
      if (n <= 0) acc
      else go(n-1, n*acc)
    go(n)
  }

  def abs(n: Int): Int =
    if (n < 0) -n
    else n

  def formatResult(name: String, n: Int, f: Int => Int) = {
    val msg = "The %s of %d is %d."
    msg.format(name, n, f(n))
  }

  def main(args: Array[String]): Unit = {
    formatResult("absolute value",-42,abs)
    formatResult("factorial",7,factorial)
  }
}
