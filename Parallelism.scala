import java.util.concurrent._



trait Par {

type Par[A] = ExecutorService => Future[A]
//The model of Par is a function which takes an executor service (lets us submit
//a callable i.e. parallel computation which returns a result) and returns a future.
//It is essentially 'val x: Future[A] = callable.execute();'

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

// map2 combines the results of two parallel computations with a binary function.
def map2[A, B, C](a: Par[A], b: Par[B])(f: (A, B) => C): Par[C]
    = { (exec: ExecutorService) => {
        val ares = run(exec)(a) 
        val bres = run(exec)(b)
        UnitFuture(f(ares.get, bres.get))
    } }

// * LAZY * //

//Fork spawns an unevaluated parallel computation. The parallel computation will not be executed until forced by run.
def fork[A](a: => Par[A]): Par[A]
    = {(exec: ExecutorService) => run(exec)(a)}
//Async instantiates a lazy Par 
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

def parMap[A, B](l: List[A])(f: A => B): Par[List[B]]
    = l.map(asyncF(f)) 

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