package RedBookTest

import RedBook.ParserCombinators._

/*
object ParserCombinators extends App {
  val P: Parsers[Int,String] = new Parsers[Int,String] {}
  import RedBookTest.ParserCombinators.P._

  val p1 = "abra" | "cadabra"
  run(listOfN(3, "ab" | "cad"))("ababcad") == Right("ababcad")
  run(listOfN(3, "ab" | "cad"))("cadabab") == Right("cadabab")
  run(listOfN(3, "ab" | "cad"))("ababab") == Right("ababab")
}
*/