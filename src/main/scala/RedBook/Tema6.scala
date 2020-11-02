package RedBook

import scala.annotation.tailrec

object functional_RNG {



  trait RNG {
    type Rand[+A] = RNG => (A, RNG)
    def nextInt: (Int, RNG)
  }

  case class SimpleRNG(seed: Long) extends RNG {
    def nextInt: (Int, RNG) = {
      val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
      val nextRNG = SimpleRNG(newSeed)
      val n = (newSeed >>> 16).toInt
      (n, nextRNG)
    }

    val int: Rand[Int] = _.nextInt

    def boolean: Rand[Boolean] = map(int)(i => i % 2 == 0)

    def nonNegativeEven: Rand[Int] =
      map(nonNegativeInt)(i => i - i % 2)

    def nonNegativeInt: Rand[Int] = {
      map(int) {
        case Int.MinValue => Int.MaxValue
        case n if n < 0 => -1*n
        case n => n
      }
    }

    /**
    EXERCISE 6.5
    Use map to reimplement double in a more elegant way. See exercise 6.2.
     */
    def double: Rand[Double] =
      map(nonNegativeInt){
        case Int.MaxValue => 1 - Double.MinPositiveValue
        case n => n.toDouble / Int.MaxValue.toDouble
      }

    def unit[A](a: A): Rand[A] =
      rng => (a, rng)

    def map[A,B](s: Rand[A])(f: A => B): Rand[B] =
      rng => {
        val (a, rng2) = s(rng)
        (f(a), rng2)
      }

    /**
    EXERCISE 6.6
    Write the implementation of map2 based on the following signature. This function
      takes two actions, ra and rb , and a function f for combining their results, and returns
      a new action that combines them:
    */
    def map2[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
      rng => {
        val (a, rng2) = ra(rng)
        val (b, rng3) = rb(rng2)
        (f(a,b),rng3)
      }

    /**
    EXERCISE 6.7
    Hard: If you can combine two RNG transitions, you should be able to combine a whole
      list of them. Implement sequence for combining a List of transitions into a single
      transition. Use it to reimplement the ints function you wrote before. For the latter,
      you can use the standard library function List.fill(n)(x) to make a list with x
      repeated n times.
     */
    def sequence[A](fs: List[Rand[A]]): Rand[List[A]] =
      fs.foldLeft( unit(List(): List[A]) )((acc,rand) => map2(rand,acc)((a,b) => a :: b))

    def intsSequence(count: Int): Rand[List[Int]] = sequence(List.fill(count)(int))

    /**
    EXERCISE 6.8
    Implement flatMap , and then use it to implement nonNegativeLessThan .
    */
    def flatMap[A,B](f: Rand[A])(g: A => Rand[B]): Rand[B] =
      rng => {
        val (a, rng2) = f(rng)
        g(a)(rng2)
      }


    def nonNegativeLessThan(n: Int): Rand[Int] =
      flatMap(nonNegativeInt) { i =>
        val mod = i % n
        if (i + (n-1) - mod >= 0) unit(mod) else nonNegativeLessThan(n)
      }

    /**
    EXERCISE 6.9
    Reimplement map and map2 in terms of flatMap . The fact that this is possible is what
      we’re referring to when we say that flatMap is more powerful than map and map2 .
    */
    def mapFM[A,B](s: Rand[A])(f: A => B): Rand[B] =
      flatMap(s)(randVal => unit(f(randVal)))

    def map2FM[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
      flatMap(ra)(a => flatMap(rb)(b => unit(f(a,b))))
  }

  object RNG {
    def int(rng: RNG): (Int, RNG) = rng.nextInt
    /**
    EXERCISE 6.1
    Write a function that uses RNG.nextInt to generate a random integer between 0 and
    Int.maxValue (inclusive). Make sure to handle the corner case when nextInt returns
    Int.MinValue , which doesn’t have a non-negative counterpart.
     */
    def nonNegativeInt(rng: RNG): (Int, RNG) = {
      val (nextInt,rng2) = rng.nextInt
      val res = nextInt match {
        case Int.MinValue => Int.MaxValue
        case n if n < 0 => -n
        case n if n >= 0 => n
      }
      (res,rng2)
    }

    /**
    EXERCISE 6.2
  Write a function to generate a Double between 0 and 1 , not including 1 . Note: You can
    use Int.MaxValue to obtain the maximum positive integer value, and you can use
    x.toDouble to convert an x: Int to a Double .
     */
    def double(rng: RNG): (Double, RNG) = {
      val (nextInt,rng2) = nonNegativeInt(rng)
      val dbl = nextInt match {
        case Int.MaxValue => 1-Double.MinPositiveValue
        case n => n.toDouble / Int.MaxValue.toDouble
      }
      (dbl,rng2)
    }

    /**
    EXERCISE 6.3
  Write functions to generate an (Int, Double) pair, a (Double, Int) pair, and a
    (Double, Double, Double) 3-tuple. You should be able to reuse the functions you’ve
  already written.
     */
    def intDouble(rng: RNG): ((Int,Double), RNG) = {
      val (int,rng2) = rng.nextInt
      val (dbl,rng3) = double(rng2)
      ((int,dbl),rng3)
    }
    def doubleInt(rng: RNG): ((Double,Int), RNG) = {
      val (dbl,rng2) = double(rng)
      val (int,rng3) = rng2.nextInt
      ((dbl,int),rng3)
    }
    def double3(rng: RNG): ((Double,Double,Double), RNG) = {
      val (dbl,rng2) = double(rng)
      val (dbl2,rng3) = double(rng2)
      val (dbl3,rng4) = double(rng3)
      ((dbl,dbl2,dbl3),rng4)
    }

    /**
    EXERCISE 6.4
  Write a function to generate a list of random integers.
     */
    def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
      @tailrec
      def loop(count: Int, acc: List[Int])(rng: RNG): (List[Int], RNG) = {
        if (count == 0) (acc,rng)
        else {
          val (int,rng2) = rng.nextInt
          loop(count-1,int :: acc)(rng2)
        }
      }
      loop(count,List())(rng)
    }
  }

  def main(args: Array[String]): Unit = {
    val rng = SimpleRNG(2)
    for (i <- 0 to 10; rngIt = SimpleRNG(i)) {
      println("nonNegativeInt:\t"+RNG.nonNegativeInt(rngIt)._1)
      println("double:\t"+RNG.double(rngIt)._1)
    }

    println("ints:\t"+RNG.ints(10)(rng))
    println("double2:\t"+rng.double)

    println("intsSequence:\t"+rng.intsSequence(3)(rng))

    println("nonNegativeLessThan:\t"+rng.nonNegativeLessThan(40)(rng))
  }
}

/**
EXERCISE 6.10
    Generalize the functions unit , map , map2 , flatMap , and sequence . Add them as meth-
      ods on the State case class where possible. Otherwise you should put them in a State
      companion object.
 */
object functionalState {
  final case class State[S,+A](run: S => (A,S)) {

    def map[B](f: A => B): State[S,B] = State(
      s => {
        val (a, s2) = this.run(s)
        (f(a), s2)
      })

    def map2[B,C](sb: State[S,B])(f: (A, B) => C): State[S,C] = State(
      s => {
        val (a, s2) = this.run(s)
        val (b, s3) = sb.run(s2)
        (f(a,b),s3)
      })

    def flatMap[B](g: A => State[S,B]): State[S,B] = State(
      s => {
        val (a, s2) = this.run(s)
        g(a).run(s2)
      })
  }

  object State {
    def unit[A,S](a: A): State[S,A] = State(s => (a,s))

    def sequence[A,S](fs: List[State[S,A]]): State[S,List[A]] =
      fs.foldLeft( State[S,List[A]]( s => (Nil,s)) )( (acc, state) => state.map2(acc)((a, list) => a :: list) )
  }

  /** EXERCISE 6.11
   * Hard: To gain experience with the use of State , implement a finite state automaton
   * that models a simple candy dispenser. The machine has two types of input: you can
   * insert a coin, or you can turn the knob to dispense candy. It can be in one of two
   * states: locked or unlocked. It also tracks how many candies are left and how many
   * coins it contains.
   *
   * The rules of the machine are as follows:
   * - Inserting a coin into a locked machine will cause it to unlock if there’s any
   * candy left.
   * - Turning the knob on an unlocked machine will cause it to dispense candy and
   * become locked.
   * - Turning the knob on a locked machine or inserting a coin into an unlocked
   * machine does nothing.
   * - A machine that’s out of candy ignores all inputs.
   *
   * The method simulateMachine should operate the machine based on the list of inputs
   * and return the number of coins and candies left in the machine at the end. For exam-
   * ple, if the input Machine has 10 coins and 5 candies, and a total of 4 candies are suc-
   * cessfully bought, the output should be (14, 1) .
   */

  sealed trait Input
  case object Coin extends Input
  case object Turn extends Input
  case class Machine(locked: Boolean, candies: Int, coins: Int)

  def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] = {
    def loop(inputs: List[Input], state: State[Machine,(Int, Int)]): State[Machine, (Int,Int)] = {
      inputs match {
        case i :: is =>
          lazy val newState = i match {
            case Coin =>
              state.flatMap(v => State(s =>
                if (s.locked) ((s.coins+1,s.candies),Machine(locked = false, coins = s.coins+1, candies = s.candies))
                else (v,s)
              ))
            case Turn =>
              state.flatMap(v => State(s =>
                if (!s.locked) ((s.coins,s.candies-1),Machine(locked = true, coins = s.coins, candies = s.candies-1))
                else (v,s)
              ))
          }
          loop(is,newState)
        case Nil => state
      }
    }
    loop(inputs,State[Machine,(Int,Int)](s => ((s.coins,s.candies),s)))
  }


  def main(args: Array[String]): Unit = {

    val inputs = List(Coin,Turn,Turn,Coin,Coin,Turn,Coin,Turn,Coin,Turn)
    val machine = Machine(locked = true,candies = 5,coins = 10)
    simulateMachine(inputs).run(machine)._1 == (14,1)
  }
}