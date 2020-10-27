package RedBook

import java.util.concurrent.{Callable, ExecutorService, Executors, Future}

import scala.concurrent.duration.TimeUnit

object Parallelism {
  type Par[A] = ExecutorService => Future[A]

  object Par {
    def unit[A](a: A): Par[A] = (es: ExecutorService) => UnitFuture(a)
    def lazyUnit[A](a: => A): Par[A] = fork(unit(a))
    def run[A](s: ExecutorService)(a: Par[A]): Future[A] = a(s)
    def fork[A](a: => Par[A]): Par[A] = es => es.submit(new Callable[A] {
      def call = a(es).get
    })
    def delay[A](fa: => Par[A]): Par[A] =
      es => fa(es)
    /**
     * EXERCISE 7.
     * Par.map2 is a new higher-order function for combining the result of two parallel com-
     * putations. What is its signature? Give the most general signature possible (don’t
     * assume it works only for Int ).
     */
    def map2[A,B,C](p1: Par[A], p2: Par[B])(f: (A,B) => C): Par[C] = (es: ExecutorService) => {
      val p1f = p1(es)
      val p2f = p2(es)
      UnitFuture(f(p1f.get, p2f.get))
    }

    /**
     * EXERCISE 7.3
     * Hard: Fix the implementation of map2 so that it respects the contract of timeouts on
     * Future .
     */ /* ???? no idea...*/
    //def map2[A,B,C](p1: Par[A], p2: Par[B])(f: (A,B) => C): Par[C] = es => ???

    def map[A,B](pa: Par[A])(f: A => B): Par[B] =
      map2(pa, unit(()))((a,_) => f(a))

    /**
     * EXERCISE 7.4
     * This API already enables a rich set of operations. Here’s a simple example: using
     * lazyUnit , write a function to convert any function A => B to one that evaluates its
     * result asynchronously.
     */
    def asyncF[A,B](f: A => B): A => Par[B] = a => lazyUnit(f(a))

    /**
     * EXERCISE 7.5
     * Hard: Write this function, called sequence . No additional primitives are required. Do
     * not call run .
     */
    def sequence[A](ps: List[Par[A]]): Par[List[A]] = ps.foldLeft(unit[List[A]](List()))((acc,elem) => map2(acc,elem)((list,a) => a :: list))

    def parMap[A,B](ps: List[A])(f: A => B): Par[List[B]] = fork {
      val fbs: List[Par[B]] = ps.map(asyncF(f))
      sequence(fbs)
    }

    /**
     * EXERCISE 7.6
     * Implement parFilter , which filters elements of a list in parallel.
     */
    def parFilter[A](as: List[A])(f: A => Boolean): Par[List[A]] = {
      val pars: List[Par[List[A]]] = as map asyncF((a: A) => if (f(a)) List(a) else List())
      map(sequence(pars))(_.flatten)
    }

    /**
     * EXERCISE 7.11
     * Implement choiceN and then choice in terms of choiceN .
     */
    def choiceN[A](n: Par[Int])(choices: List[Par[A]]): Par[A] =
      es => choices(run(es)(n).get)(es)

    def choice[A](cond: Par[Boolean])(t: Par[A], f: Par[A]): Par[A] =
      choiceN(map(cond)(x => if (x) 0 else 1))(List(t,f))

    def choiceMap[K,V](key: Par[K])(choices: Map[K,Par[V]]): Par[V] =
      es => choices(key(es).get)(es)

    /**
     * EXERCISE 7.13
     * Implement this new primitive chooser , and then use it to implement choice and
     * choiceN .
     */
    def chooser[A,B](pa: Par[A])(choices: A => Par[B]): Par[B] =
      es => choices(pa(es).get)(es)

    def choiceNChooser[A](n: Par[Int])(choices: List[Par[A]]): Par[A] = chooser(n)(choices)
    def choiceChooser[A](cond: Par[Boolean])(t: Par[A], f: Par[A]): Par[A] = chooser(map(cond)(x => if (x) 0 else 1))(List(t,f))

    /**
     * EXERCISE 7.14
     * Implement join . Can you see how to implement flatMap using join ? And can you
     * implement join using flatMap ?
     */
    def join[A](a: Par[Par[A]]): Par[A] = es => run(es)(run(es)(a).get())

    def flatMap[A,B](a: Par[A])(f: A => Par[B]): Par[B] = es => join(map(a)(x => f(x)))(es)

    def joinFlatMap[A](a: Par[Par[A]]): Par[A] = flatMap(a)(x => x)

    /* Tema 8 additions */
    def equal[A](p: Par[A], p2: Par[A]): Par[Boolean] =
      Par.map2(p,p2)(_ == _)
  }


  private case class UnitFuture[A](get: A) extends Future[A] {
    def isDone = true
    def get(timeout: Long, units: TimeUnit): A = get
    def isCancelled = false
    def cancel(evenIfRunning: Boolean): Boolean = false
  }


  def main(args: Array[String]): Unit = {
    val filterOp = Par.parFilter(List(1, 2, 3, 4, 5))(_ < 4)
    val executorService = Executors.newCachedThreadPool()
    val result = Par.run(executorService)(filterOp).get()
    println(result)


    val executorService2 = Executors.newFixedThreadPool(2)
    val choice = Par.choiceChooser(Par.unit(true))(Par.unit(1), Par.unit(2))
    println(choice.apply(executorService2).get())
  }
}
