package Parallelism

import java.util.concurrent._, ExecutorService._


object folds{
  def foldLeft[A,B](l: List[A], z: B)(f: (B, A) => B) : B = l match {
    case Nil => z
    case ::(x, xs) => foldLeft(xs, f (z, x)) (f)
  }
  def foldRight[A,B](l: List[A], z: B)(f: (A, B) => B): B = l match {
    case Nil => z
    case ::(x, xs) =>  f(x, foldRight(xs, z)(f))
  }
}

import folds._
import scala.List
import List._
import scala.collection.immutable.List._
import language.implicitConversions

// trait ExecutorService {
//   def submit[A](a: Callable[A]): Future[A]
// }
// trait Future[A] {
//   def get: A
//   def get(timeout: Long, unit: TimeUnit): A
//   def cancel(evenIfRunning: Boolean): Boolean
//   def isDone: Boolean
//   def isCancelled: Boolean
// }

object Par {
    type ExecutorService = java.util.concurrent.ExecutorService
    type Future[A] = java.util.concurrent.Future[A]
    type Par[A] = ExecutorService => Future[A]

    case class UnitFuture[A](a: A) extends Future[A]{
        def get: A 
            = a
        def get(timeout: Long, unit: TimeUnit): A 
            = a
        def cancel(evenIfRunning: Boolean): Boolean 
            = false 
        def isCancelled 
            = false 
        def isDone
            = true
    }

    // * LAZY * //

    def fork[A](a: => Par[A]): Par[A]
        = {(exec: ExecutorService) =>  exec.submit(new Callable[A] {     // Submits 1st callable 
            def call: A = a(exec).get // a(exec) is also a Future[A]    // Submits 2nd callable
            
        }): Future[A]}
    //Async instantiates a lazy Par (unit)
    def async[A](a: => A): Par[A] 
        = fork(unit(a))

    def asyncF[A, B](f: A => B): A => Par[B]
        = { (a: A) => fork(unit(f(a))) }

    // * STRICT * //

    // unit injects a constant into a parallel computation.
    def unit[A](a: A): Par[A]
        = { (exec: ExecutorService) => UnitFuture(a)}
        // = { s: ExecutorService => Future(a) }

    // run will strictly evaluate a parallel computation, and return a value
    def run[A](s: ExecutorService)(a: Par[A]): Future[A] 
        = a(s)

    def product[A, B](fa: Par[A], fb: Par[B]): Par[(A,B)]
        = { (exec: ExecutorService) => UnitFuture(fa(exec).get, fb(exec).get) }

    def map[A, B](fa: Par[A])(f: A => B): Par[B]
        = { (exec: ExecutorService) => UnitFuture(f (fa(exec).get) )}

    // map2 combines the results of two parallel computations with a binary function.
    def map2[A, B, C](a: Par[A], b: Par[B])(f: (A, B) => C): Par[C]
        = { (exec: ExecutorService) => {
            val ares = run(exec)(a) 
            val bres = run(exec)(b)

            UnitFuture(f(ares.get, bres.get))
        } }

    //Map an async lazy function over each element
    def parMap[A, B](l: List[A])(f: A => B): Par[List[B]]
        = fork(sequence(l.map(asyncF(f))))

    def sequence[A](l: List[Par[A]]): Par[List[A]] 
        = foldRight(l, async(List()): Par[List[A]] )((parA, parLA) => {(exec: ExecutorService) => 
            UnitFuture(::(parA(exec).get, parLA(exec).get))
        })

    def parFilter[A](l: List[A])(f: A => Boolean): Par[List[A]]
        = { val pars: List[Par[List[A]]] 
                = l.map(asyncF( (a: A) => if(f(a)) List(a) else List() )) 
            val swap: Par[List[List[A]]] = sequence(pars)
            map(swap)((ls: List[List[A]]) => ls.flatten)
        }
    def choiceN[A](a: Par[Int])(choices: List[Par[A]]): Par[A]
        = { (exec: ExecutorService) => { val i = a(exec).get
                                         choices(i)(exec)}
        }
    def join[A](a: Par[Par[A]]): Par[A]
        = { (exec: ExecutorService) => { ((map(a)(_(exec)))(exec)).get  }}

    def flatMap[A, B](a: Par[A])(f: A => Par[B]): Par[B]
        = { (exec: ExecutorService) => {    val as: A = (a(exec).get)
                                            (f(as))(exec)}}
}

object Main extends App {
    import Par._
    val es = Executors.newFixedThreadPool(5)
    val ls = List(1,2,3,4,5)
    // for {
    //     i <- ls 
    // } yield run(es)(async(println(i)))
    //  for {
    //     i <- ls 
    // } yield run(es)(unit(println(i)))
    run(es)(parMap(ls)(println(_)))
    // println(s)
    val res = run(es)(async(println()))
    
}



// object Par {
//     import Par._

//     def sum(as: IndexedSeq[Int]): Int =
//         if (as.size <= 1) 
//             //Just(head).getOrElse(0) == head or 0
//             as.headOption getOrElse 0
//         else {
//             val (l,r) = as.splitAt(as.length/2)
//             // Unit performs async operation to return a Par[Int] to 'get'.
//             val sumL: Par[Int] = Par.unit(sum(l))
//             val sumR: Par[Int] = Par.unit(sum(r))
//             // 'Get' explicitly waits for 'unit', exposing the side effect. 
//             // This is because 'get' *requires* an Int to be returned.
//             Par.get(sumL) + Par.get(sumR)
//         }
//     // If we don't call 'get', this implies our sum function must return a Par[Int]
//     // instead.
//     def sum(as: IndexedSeq[Int]): Par[Int] =
//         if (as.size <= 1)
//             Par.unit(as.headOption getOrElse 0)
//         else {
//             val (l,r) = as.splitAt(as.length/2)
            
//             Par.map2(sum(l), sum(r))(_ + _)
//         }
//     // Should this have its arguments evaluated lazily?
//     // If they are strict, the LHS will be evaluated first so no parallelism
//     // Thus lazy arguments would be best for both arguments 'a' and 'b'.
//     def map2[A, B, C](a: => Par[A], b: => Par[B])(f: (A, B) => C)

//     //Fork allows us to leave map2 strict, so that we can wrap up arguments
//     //if we want. Fork solves the problem of instantiating parallel computations
//     //too strictly, and makes parallelism more *explicit*.
// }