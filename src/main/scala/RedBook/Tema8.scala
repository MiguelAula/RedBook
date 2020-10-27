package RedBook

/**
 * IMPORTANT: alguns dels exercicis d'aquest tema estan al fitxer test/RedBook/Tema8 (scalaCheck funciona sota el directori test)
 */

import java.util.concurrent.{ExecutorService, Executors}

import RedBook.Parallelism.Par
import RedBook.Parallelism.Par.equal
import RedBook.functionalState._
import RedBook.functional_RNG._

import scala.annotation.tailrec

object PropImplementation {
  /**
   * EXERCISE 8.3
   * Assuming the following representation of Prop , implement && as a method of Prop .
   */
  /*
  trait Prop {
    def check: Boolean
    def &&(p: Prop): Prop = new Prop {
      def check: Boolean = this.check & p.check
    }
  }
  */

  case class SGen[+A](forSize: Int => Gen[A]) {
    def apply(n: Int): Gen[A] = forSize(n)
    /**
    EXERCISE 8.11
    Not surprisingly, SGen at a minimum supports many of the same operations as Gen ,
    and the implementations are rather mechanical. Define some convenience functions
    on SGen that simply delegate to the corresponding functions on Gen .
    */
    def map[B](f: A => B): SGen[B] = SGen(forSize(_) map f)

    def flatMap[B](f: A => SGen[B]): SGen[B] = {
      val forSize2 = (n: Int) => forSize(n) flatMap {f(_)(n)}
      SGen(forSize2)
    }
  }
  case class Gen[+A](sample: State[RNG,A]) {
    def map[B](f: A => B): Gen[B] = Gen(this.sample.map(a => f(a)))

    /**
     * EXERCISE 8.6
     * Implement flatMap , and then use it to implement this more dynamic version of
     * listOfN . Put flatMap and listOfN in the Gen class.
     */
    def flatMap[B](f: A => Gen[B]): Gen[B] = Gen(this.sample.flatMap(a => f(a).sample))
    def listOfN(size: Gen[Int]): Gen[List[A]] = size flatMap (n => Gen.listOfN(n, this))

    /**
    EXERCISE 8.10
    Implement helper functions for converting Gen to SGen . You can add this as a method
    on Gen .
    */
    def unsized: SGen[A] = SGen { _ => this }

    def listOfAll: Option[List[A]] = {
      val sample = this.sample.run(SimpleRNG(0))._1
      sample match {
        case _: Boolean => Some(List(true,false).asInstanceOf[List[A]])
        case _: Byte =>
          @tailrec
          def allBytesList(acc: List[Byte] = List(Byte.MinValue)): List[Byte] = acc match {
            case x :: _ =>
              if (x == Byte.MaxValue) acc
              else allBytesList((x+1).toByte :: acc)
            case _ => allBytesList(List(Byte.MinValue)) //just in case
          }
          Some(allBytesList().asInstanceOf[List[A]])
        case _ => None
      }
    }

    def map2[B,C](g: Gen[B])(f: (A,B) => C): Gen[C] = this.flatMap(a => g.map(b => f(a,b)))

    def **[B](g: Gen[B]): Gen[(A,B)] =
      (this map2 g)((_,_))
  }
  /** To enable a ** b as patternmatching for Gen*/
  object ** {
    def unapply[A,B](p: (A,B)) = Some(p)
  }

  object Gen {
    def map2[A,B,C](g1: Gen[A], g2: Gen[B])(f: (A,B) => C): Gen[C] = g1.flatMap(a => g2.map(b => f(a,b)))

    /**
     * EXERCISE 8.7
     * Implement union , for combining two generators of the same type into one, by pulling
     * values from each generator with equal likelihood.
     */
    def union[A](g1: Gen[A], g2: Gen[A]): Gen[A] = boolean flatMap (b => if (b) g1 else g2)

    /**
     * EXERCISE 8.8
     * Implement weighted , a version of union that accepts a weight for each Gen and gener-
     * ates values from each Gen with probability proportional to its weight.
     */
    def weighted[A](g1: (Gen[A],Double), g2: (Gen[A],Double)): Gen[A] = {
      Gen(State(RNG.double)) flatMap (i =>
        (g1,g2) match {
          case ((aGen,aWeight),(bGen,bWeight)) =>
            val aLimit = aWeight.abs / (aWeight.abs + bWeight.abs)
            if (aWeight < aLimit) aGen
            else bGen
        })
    }

    /**
     * EXERCISE 8.4
     * Implement Gen.choose using this representation of Gen . It should generate integers in
     * the range start to stopExclusive . Feel free to use functions you’ve already written.
     */
    def choose(start: Int, stopExclusive: Int): Gen[Int] =
      Gen(State(RNG.nonNegativeInt).map(n => start + n % (stopExclusive - start)))

    def int: Gen[Int] = Gen(State(RNG.int))

    def positiveInt: Gen[Int] = Gen(State(RNG.nonNegativeInt))

    def char: Gen[Char] = {
      val upperGen = choose(65,90).map(i => i.toChar)
      val lowerGen = choose(97,122).map(i => i.toChar)
      union(upperGen,lowerGen)
    }

    /**
     * EXERCISE 8.5
     * Let’s see what else we can implement using this representation of Gen . Try implement-
     * ing unit , boolean , and listOfN .
     */
    def unit[A](a: => A): Gen[A] = Gen(State.unit(a))
    def boolean: Gen[Boolean] = Gen(State(RNG.nonNegativeInt).map(n => n % 2 == 0))
    /** Generates lists of length n using the generator g: */
    def listOfN[A](n: Int, g: Gen[A]): Gen[List[A]] = Gen(State.sequence(List.fill(n)(g.sample)))


    /**
     * EXERCISE 8.12
     * Implement a listOf combinator that doesn’t accept an explicit size. It should return an
     * SGen instead of a Gen . The implementation should generate lists of the requested size.
     */
    def listOf[A](g: Gen[A]): SGen[List[A]] = SGen(
      (n: Int) => Gen(State.sequence(List.fill(n)(g.sample)))
    )

    /**
     * EXERCISE 8.13
     * Define listOf1 for generating nonempty lists, and then update your specification of
     * max to use this generator.
     */
    def listOf1[A](g: Gen[A]): SGen[List[A]] = SGen(
      (n: Int) => Gen(State.sequence(List.fill(if (n == 0) 1 else n)(g.sample)))
    )
  }

  type TestCases = Int
  type FailedCase = String
  type SuccessCount = Int
  sealed trait Result {
    def isFalsified: Boolean
  }
  case object Passed extends Result {
    def isFalsified = false
  }
  case class Falsified(failure: FailedCase, successes: SuccessCount) extends Result {
    def isFalsified = true
  }
  case object Proved extends Result {
    def isFalsified = false
  }
  type MaxSize = Int

  case class Prop(run: (MaxSize,TestCases,RNG) => Result, tag: String  = "", exhaustive: Boolean = false) {
    /**
     * EXERCISE 8.9
     * Now that we have a representation of Prop , implement && and || for composing Prop
     * values. Notice that in the case of failure we don’t know which property was responsi-
     * ble, the left or the right. Can you devise a way of handling this, perhaps by allowing
     * Prop values to be assigned a tag or label which gets displayed in the event of a failure?
     */
    def &&(p: Prop): Prop = Prop {
      (max, n, rng) =>
        this.run(max, n, rng) match {
          case res if !res.isFalsified => p.run(max, n, rng)
          case err => err
        }
    }

    def ||(p: Prop): Prop = Prop {
      (max, n, rng) =>
        this.run(max, n, rng) match {
          case Falsified(msg, _) => p.tag(msg).run(max, n, rng)
          case x => x
        }
    }

    def tag(s: String): Prop = Prop(this.run,s)
  }
  object Prop {
    def check(p: => Boolean): Prop = Prop { (_, _, _) =>
      if (p) Proved else Falsified("()", 0)
    }
    def run(
      p: Prop,
      maxSize: Int = 100,
      testCases: Int = 100,
      rng: RNG = SimpleRNG(System.currentTimeMillis)
   ): Unit =
      p.run(maxSize, testCases, rng) match {
        case Falsified(msg, n) =>
          println(s"! Falsified after $n passed tests:\n $msg")
        case Passed =>
          println(s"+ OK, passed $testCases tests.")
        case Proved =>
          println(s"+ OK, proved property.")
      }

    /**
     * EXERCISE 8.15
     * Hard: A check property is easy to prove conclusively because the test just involves eval-
     * uating the Boolean argument. But some forAll properties can be proved as well. For
     * instance, if the domain of the property is Boolean , then there are really only two cases
     * to test. If a property forAll(p) passes for both p(true) and p(false) , then it is
     * proved. Some domains (like Boolean and Byte ) are so small that they can be exhaus-
     * tively checked. And with sized generators, even infinite domains can be exhaustively
     * Property-based testing
     * checked up to the maximum size. Automated testing is very useful, but it’s even better
     * if we can automatically prove our code correct. Modify our library to incorporate this kind
     * of exhaustive checking of finite domains and sized generators. This is less of an exer-
     * cise and more of an extensive, open-ended design project.
     */
    def forAll[A](as: Gen[A])(f: A => Boolean): Prop = Prop {
      (_,n,rng) =>
        val (valSequence,successCase) = as.listOfAll match {
          case Some(l) => (l.zipWithIndex,Proved)
          case None => (randomStream(as)(rng).zip(Stream.from(0)).take(n),Passed)
        }
        valSequence.map {
          case (a, i) => try {
            if (f(a)) successCase else Falsified(a.toString, i)
          } catch { case e: Exception => Falsified(buildMsg(a, e), i) }
        }.find(_.isFalsified).getOrElse(successCase)
    }

    def forAll[A](g: SGen[A])(f: A => Boolean): Prop = forAll(g(_))(f)
    def forAll[A](g: Int => Gen[A])(f: A => Boolean): Prop = Prop {
      (max,n,rng) =>
        val casesPerSize = (n + (max - 1)) / max
        val props: Stream[Prop] =
          Stream.from(0).take((n min max) + 1).map(i => forAll(g(i))(f))
        val prop: Prop =
          props.map(p => Prop { (max, _, rng) =>
            p.run(max, casesPerSize, rng)
          }).toList.reduce(_ && _)
        prop.run(max,n,rng)
    }

    def randomStream[A](g: Gen[A])(rng: RNG): Stream[A] =
      Stream.unfold(rng)(rng => Some(g.sample.run(rng)))

    def buildMsg[A](s: A, e: Exception): String =
      s"test case: $s\n" +
      s"generated an exception: ${e.getMessage}\n" +
      s"stack trace:\n ${e.getStackTrace.mkString("\n")}"

    val S = Gen.weighted(
      Gen.choose(1,4).map(Executors.newFixedThreadPool) -> .75,
      Gen.unit(Executors.newCachedThreadPool) -> .25)
    def forAllPar[A](g: Gen[A])(f: A => Par[Boolean]): Prop =
      forAll(S ** g) { case s ** a => f(a)(s).get }
    def checkPar(p: => Par[Boolean]): Prop = Prop { (_, _, _) =>
      val result = Par.map(p)(b => if (b) Proved else Falsified("()",0))
      S.map(ES => Par.run(ES)(result).get).sample.run(SimpleRNG(System.currentTimeMillis()))._1
    }
  }
}

