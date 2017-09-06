
trait RNG {
    // Essentially a cyclic definition of RNG/State //
    type Rand[+A] = RNG => (A, RNG)
    
    def nextInt: (Int, RNG);
    
}

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

object RNG {
    import folds._

    type Rand[+A] = RNG => (A, RNG)
    def simple(seed: Long): RNG = new RNG {
        /**
        This allows us to take our state s to (a, s). 
        / */
        def nextInt = {
            val seed2 = (seed*0x5DEECE66DL + 0xBL) &
                        ((1L << 48) - 1)
            ((seed2 >>> 16).asInstanceOf[Int],
            simple(seed2))      // Here our state is our value wrapped up in 
        }                       // a function 'simple'.
    }                           
    
    def positiveInt(rng: RNG): (Int, RNG) = {
        val (a, s) = rng.nextInt 
        (a.abs, s)
    }
    def double(rng: RNG): (Double, RNG) = {
        val (a, s) = rng.nextInt 
        (a.abs, s)
    }
    def intDouble(rng: RNG): ((Int, Double), RNG) = {
        val (a1, s1) = rng.nextInt
        val (a2, s2) = double(s1)
        ((a1, a2), s2)
    }
    def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
        if(count == 0)
            (List(), rng)
        else {
            val (a1, s1) = rng.nextInt
            val (as, s2) = ints(count - 1)(s1)
            (::(a1, as), s2)
        }
    }
    
    // return: a => (s => (a, s))   //
    def unit[A](a: A): Rand[A] = 
        rng => (a, rng)
    // map: When given s, runState on s to give (a, s), then map f on value a
    def map[A,B](s: Rand[A])(f: A => B): Rand[B] =
        rng => {
            val (a, rng2) = s(rng)
            (f(a), rng2)
        }
    val int: Rand[Int] = _.nextInt
    // Defining state processors (s => (a, s))
    def positiveMax(n: Int): Rand[Int] = {
        rng: RNG => {
            val (a, rng2) = rng.nextInt
            (a/(a-n).abs, rng2)
        }
    }
    def doubleM: Rand[Double] = 
        map(rng => rng.nextInt)(a => a.asInstanceOf[Double])
    
    def map2[A, B, C](s1: Rand[A], s2: Rand[B])(f: (A, B) => C): Rand[C] = {
        rng => {
            val (a, st1) = s1(rng)
            val (b, st2) = s2(st1)
            (f(a,b), st2)
        }
    }
    def sequence[A](fs: List[Rand[A]]): Rand[List[A]] = {
        foldRight(fs, unit(List[A]())) ((nxt, acc) => {
            state: RNG => {
                val (a1, s1) = acc(state)
                val (a2, s2) = nxt(s1)
                (::(a2, a1), s2)
            }
        }) 
    }
    def flatMap[A, B](f: Rand[A])(g: A => Rand[B]): Rand[B] = {
        rng => { 
            val (a, s) = f(rng)
            g(a)(s)
        }
    }
    def posInt: Rand[Int] = {
        flatMap(s1 => s1.nextInt)((a: Int) => {
            if (a != Int.MinValue) {
                s => (a.abs, s)
            }
            else {
                s => s.nextInt
            }
        })
    }
}

import State._
import folds._

case class State[S, +A](runState: S => (A, S)) {
    def map[B](f: A => B): State[S, B] =
        flatMap(a => unit(f(a)))
    def map2[B,C](sb: State[S, B])(f: (A, B) => C): State[S, C] =
        flatMap(a => sb.map(b => f(a, b)))
    def flatMap[B](f: A => State[S, B]): State[S, B] = State(s => {
        val (a, s1) = runState(s)
        f(a).runState(s1)
    })
    
    
}

object State {
    def unit[S, A](a: A): State[S, A] =
        State(s => (a, s))

    def sequence[S,A](ls: List[State[S, A]]): State[S, List[A]] = 
        ls.foldRight(unit[S, List[A]](List()))( (nxt, acc) => State{
            state: S => {
                val (a1, s1) = acc.runState(state)
                val (a2, s2) = nxt.runState(s1)
                (::(a2, a1), s2)
            }
    })

    def get[S]: State[S, S] = State{
        s: S => (s, s)
    } 

    def set[S]: (S => State[S, _]) = { 
        s: S => State{ _ => ((), s) } 
    }

}

object Pure {
    import RNG._
    val rng: RNG = simple(0)

}