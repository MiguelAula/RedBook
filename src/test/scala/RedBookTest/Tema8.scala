package RedBookTest

import java.util.concurrent.{ExecutorService, Executors}

import RedBook.Parallelism.Par
import RedBook.Parallelism.Par.equal
import RedBook.functional_RNG.SimpleRNG
import RedBook.PropImplementation.Gen
import RedBook.PropImplementation.Prop
import RedBookTest.testingGen.rng

import scala.annotation.tailrec


object testingGen extends App {
  val rng = SimpleRNG(System.currentTimeMillis())
  /**
   * If we can generate a single Int in some range, do we need a new primitive to
   * generate an (Int,Int) pair in some range?
   * ANSWER: NO
   */
  val pairRangeGen: Gen[(Int,Int)] = Gen.choose(0,100).flatMap(a => Gen.choose(0,100).map(b => (a,b)))
  println(pairRangeGen.sample.run(rng)._1)
  /* but we could also make a map2 function to generalize this type of logic */
  val pairRangeGen_map2: Gen[(Int,Int)] = Gen.map2(Gen.choose(0,100),Gen.choose(0,100))((a,b) => (a,b))
  println(pairRangeGen_map2.sample.run(rng)._1)
  /**
   * Can we produce a Gen[Option[A] from a Gen[A] ? What about a Gen[A] from a
   * Gen[Option[A] ?
   * ANSWER: ?
   * */

  /**
   * Can we generate strings somehow using our existing primitives?
   * ANSWER: YES, also we can define Gen.char
   * */
  val unitStr = Gen.unit("hola")
  println(unitStr.sample.run(rng)._1)
  val randStr = Gen.listOfN(10,Gen.char).map(l => l.mkString)
  println(randStr.sample.run(rng)._1)

  val randStr2 = Gen.char.listOfN(Gen.unit(10)).map(l => l.mkString)
  println(randStr2.sample.run(rng)._1)


  println(Gen.choose(1,100).listOfN(Gen.choose(1,10)).sample.run(rng))
}


object testingProps extends App {
  val rng = SimpleRNG(System.currentTimeMillis())
  println(
    Prop.forAll(Gen.listOfN(10,Gen.positiveInt))(l => l.exists(x => x < 10)).run(100,100,rng)
  )

  //listOf
  val smallInt = Gen.choose(-10,10)
  val maxProp = Prop.forAll(Gen.listOf(smallInt)) { ns =>
    val max = ns.max
    !ns.exists(_ > max)
  }
  //println(Prop.run(maxProp))  //fails for empty list

  //listOf1
  val maxProp2 = Prop.forAll(Gen.listOf1(smallInt)) { ns =>
    val max = ns.max
    !ns.exists(_ > max)
  }
  println(Prop.run(maxProp2)) //succeeds because listOf1 makes sure that the empty list is not used

  /**
   * EXERCISE 8.14
   * Write a property to verify the behavior of List.sorted ( API docs link: http://mng.bz/
   * Pz86), which you can use to sort (among other things) a List[Int] . 7 For instance,
   * List(2,1,3).sorted is equal to List(1,2,3) .
   */
  val checkSorted = Prop.forAll(Gen.listOf1(smallInt)) { l =>
    /**
     *  This function traverses a list and checks if a given predicate f holds for every pair of consecutive elements of the list
     */
    @tailrec
    def predicateHolds[A](list: List[A], last: Option[A] = None)(f: (A,A) => Boolean): Boolean = list match {
      case x :: xs =>
        last match {
          case Some(lastX) =>
            if (f(lastX,x)) predicateHolds(xs,Some(x))(f)
            else false
          case None => predicateHolds(xs,Some(x))(f)
        }
      case Nil => true
    }

    predicateHolds(l.sorted)(_ <= _)
  }
  println("checkSorted\n" + Prop.run(checkSorted))


  val boolProp = Prop.forAll(Gen.boolean){
    b => (b && b) == b
  }
  println("boolProp\n" + Prop.run(boolProp))

  /* Par testing */
  import Prop._

  val ES: ExecutorService = Executors.newCachedThreadPool
  val p1 = forAllPar(Gen.unit(Par.unit(1)))(i =>
    Par.unit(Par.map(i)(_ + 1)(ES).get == Par.unit(2)(ES).get))
  println("Parcheck v1\n" + run(p1))


  val p2 = checkPar {
    equal (
      Par.map(Par.unit(1))(_ + 1),
      Par.unit(2)
    )
  }
  println("Parcheck v2\n" + run(p2))

  val pint = Gen.choose(0,10) map (Par.unit(_))
  val p4 =
    forAllPar(pint)(n => equal(Par.map(n)(y => y), n))
  println("Parcheck v4\n" + run(p4))

  /**
   * EXERCISE 8.16
   * Hard: Write a richer generator for Par[Int] , which builds more deeply nested parallel
   * computations than the simple ones we gave previously.
   */
  val pint2: Gen[Par[Int]] = Gen.choose(-100, 100).listOfN(Gen.choose(0, 20)).map(l =>
    l.foldLeft(Par.unit(0))((p, i) =>
      Par.fork { Par.map2(p, Par.unit(i))(_ + _) }))

  /**
   * EXERCISE 8.17
   * Express the property about fork from chapter 7, that fork(x) == x .
   */
  /*val forkProp = ???
  println("forkprop\n" + Prop.run(forkProp))*/
}

