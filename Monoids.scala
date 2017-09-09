import Parallelism.Par._
import scala.language.implicitConversions
import scala.language.higherKinds
import java.util.concurrent._, ExecutorService._

trait Monoid[A] {
    def mappend(a1: A, a2: A): A 
    def mempty: A 
    def mconcat(as: List[A], a: Monoid[A]): A = 
        as.foldRight(a.mempty)((bm, am) => a.mappend(bm, am))
    def foldMap[A,B](as: List[A], m: Monoid[B])(f:A => B): B =
        as.foldRight(m.mempty)((a:A, b:B) => m.mappend(b, f(a)))
    def foldMapV[A, B](as: List[A], m: Monoid[B])(f:A => B): B = {
        if(as.isEmpty)
            m.mempty
        else if(as.length == 1)
            f(as(0))
        else{
            val tuple: (List[A], List[A]) = as.splitAt(as.length/2)
            m.mappend(foldMapV(tuple._1, m)(f), 
                      foldMapV(tuple._2, m)(f))
        } 
    }
}

object ProductMonoid {
    def productMonoid[A, B](a: Monoid[A], b: Monoid[B]) = new Monoid[(A, B)]{
        def mappend(a1 : (A, B), a2: (A, B)): (A, B)
            = ((a.mappend(a1._1, a2._1)), (b.mappend(a1._2, a2._2)))
        def mempty: (A, B)
            = (a.mempty, b.mempty)
    }
}

trait Monoid2[A] {
    def mappend(a1: A, a2: A): A 
    def mempty: A 
    def mconcat(as: List[A]): A = 
        as.foldRight(mempty)((bm, am) => mappend(bm, am))
    def foldMap[B](as: List[A], m: Monoid2[B])(f:A => B): B =
        as.foldRight(m.mempty)((a:A, b:B) => m.mappend(b, f(a)))
    def foldMapV[B](as: List[A], m: Monoid2[B])(f:A => B): B = {
        if(as.isEmpty)
            m.mempty
        else if(as.length == 1)
            f(as(0))
        else{
            val tuple: (List[A], List[A]) = as.splitAt(as.length/2)
            m.mappend(foldMapV(tuple._1, m)(f), 
                      foldMapV(tuple._2, m)(f))
        } 
    }
}

trait Maybe[+A]
case class Just[+A](a: A) extends Maybe[A]
case object Nothing extends Maybe[Nothing]

object Maybe {
    implicit class maybeMonoid[A](a: Maybe[A]) extends Monoid[Maybe[A]]{
        def mappend(a1: Maybe[A], a2: Maybe[A]): Maybe[A] = (a1, a2) match {
            case (Just(a), _) => Just(a)
            case (_, Just(a)) => Just(a)
            case _ => Nothing
        }
        def mempty: Maybe[A] = Nothing
    }
    val x: Just[Int] = Just(5)
    x.mempty
}


object String {
    implicit def stringMonoid = new Monoid[String]{
        def mappend(a: String, b: String): String = a ++ " " ++ b 
        def mempty: String = "" 
    }
}

object Arithmetic {

    implicit val intAdd = new Monoid[Int]{
        def mappend(a: Int, b: Int) = a + b
        def mempty = 0
    }
    implicit val intMult = new Monoid[Int]{
        def mappend(a: Int, b: Int) = a*b
        def mempty = 0
    }
    
    implicit class boolOr(b: Boolean) extends Monoid[Boolean]{
        def mappend(a: Boolean, b: Boolean) = a || b
        def mempty = false
    }
    implicit object boolAnd extends Monoid[Boolean]{
        def mappend(a: Boolean, b: Boolean) = b && b
        def mempty = true
    }
} 

object SortInt {
    type SortInt = Maybe[(Int, Int, Boolean)]
    implicit class sortIntMonoid(i: SortInt) extends Monoid[SortInt]{
        def mappend(a: SortInt, b: SortInt) = (a, b) match {
            case (Just((min1, max1, bool1)), Just((min2, max2, bool2))) => {
                if(min1 <= min2 && max1 <= max2) 
                    Just((min1, max2, bool1 && bool2))
                else if(min1 <= min2)
                    Just((min1, max1, false))
                else 
                    Just((min2, max1, false))
            }
            case (x, Nothing) => x
            case (Nothing, x) => x
        }
        def mempty = Nothing
    }
}

sealed trait WC
case class Stub(chars: String) extends WC
case class Part(lStub: String, words: Int, rStub: String) extends WC

object WC {
    // Type Class
    val exec: ExecutorService = Executors.newFixedThreadPool(10)
    type ExecutorService = java.util.concurrent.ExecutorService
    type Future[A] = java.util.concurrent.Future[A]
    type Par[A] = ExecutorService => Future[A]
    implicit class wcMonoid(wc: WC) extends Monoid[WC]{
        def mappend(a: WC, b: WC): WC = (a,b) match {
        case (Part(l1, w1, r1), Part(l2,w2,r2)) =>
            if(r1.isEmpty && l2.isEmpty) 
                Part(l1 ++ r1 ++ l2, w2+w1, r2)
            else 
                Part(l1 ++ r1 ++ l2, w2+w1+1, r2)
        case (Stub(s), Part(l2,w2,r2))      => Part(s+l2, w2, r2)
        case (Part(l1, w1, r1), Stub(s))    => Part(l1, w1, r1++s)
        case (Stub(s1), Stub(s2))           => Stub(s1 + s2)
        }
        def mempty: WC = Stub("")
    }    
    // Change Monoid[m] to Monoid[Par[m]]
    def monoidToPar[A](m: Monoid[A]): Monoid[Par[A]] = new Monoid[Par[A]]{
        def mappend(a1: Par[A], a2: Par[A]) = {
            map2(a1, a2)(m.mappend)
        }
        def mempty = unit(m.mempty)
    }
    // Change string to list of WC
    def strTo(s: String): List[WC] = {
        s.map(c => {if (c == ' ') Part("", 0, "")
                         else Stub(c.toString)} ).toList
    }

    // we perform the mapping and the reducing both in parallel
    def parFoldMap[A,B](v: List[A], m: Monoid[B])(f: A => B): Par[B] = {
       val parListofMonoidParWC : Par[List[B]] = parMap(v)(f)               //par[list[monoid[par[wc]]]]
       flatMap(parListofMonoidParWC)(bs => unit(m.foldMap(bs, m)(x => x)))  //par[par[wc]]
       // flatMap(parMap(v)(f))(bs => m.foldMapV(bs, monoidToPar(m))(b => unit(b)))
    }
    def wcToPar(wc: WC): Par[WC] = {
        val parparWC: Par[Par[WC]] = (parFoldMap(strTo("nothing to see here then if only i were alive"), monoidToPar((wc)))(x => unit(x)))
        val parWC: Par[WC] = join(parparWC)
        parWC
    }
}

object Main extends App{
    import WC._
}

trait Foldable[F[_]]{
    def foldRight[A, B](as: F[A])(b: B)(f: (A, B) => B): B 
    def foldLeft[A, B](as: F[A])(b: B)(f: (B, A) => B): B
    def foldMap[A, B <: Monoid[B]](as: F[A])(f: A => B)(b: B): B       
    def concatenate[A](as: F[A])(m: Monoid[A]): A =
        foldLeft(as)(m.mempty)(m.mappend)
    def toList[A](fa: F[A]): List[A]
        = foldRight(fa)(List(): List[A])((a, acc) => ::(a, acc))
}

object FList{
    implicit class foldableList[A](l: List[A]) extends Foldable[List]{
        def foldRight[A, B](as: List[A])(b: B)(f: (A, B) => B): B = as match {
            case ::(x, xs) => foldRight(xs)(f(x, b))(f)
            case Nil => b
        }
        def foldLeft[A, B](as: List[A])(b: B)(f: (B, A) => B): B = as match {
            case ::(x, xs) => f(foldLeft(xs)(b)(f), x)
            case Nil => b
        }
        def foldMap[A, B <: Monoid[B]](as: List[A])(f: A => B)(b: B): B
            = foldRight(as.map(f))(b.mempty)(b.mappend)
    }
}

object FSeq{
    implicit class foldableSeq[A](l: IndexedSeq[A]) extends Foldable[IndexedSeq]{
        def foldRight[A, B](as: IndexedSeq[A])(b: B)(f: (A, B) => B): B = {
            if (as.length < 1) b
            else foldRight(as.drop(1))(f(as(0), b))(f)
        }
        def foldLeft[A, B](as: IndexedSeq[A])(b: B)(f: (B, A) => B): B = {
            if (as.length < 1) b
            else f(foldLeft(as.drop(1))(b)(f), as(1))
        }
        def foldMap[A, B <: Monoid[B]](as: IndexedSeq[A])(f: A => B)(b: B): B
            = foldRight(as.map(f))(b.mempty)(b.mappend)
    }
}

object FStream{
    implicit class foldableStream[A](l: Stream[A]) extends Foldable[Stream]{
        def foldRight[A, B](as: Stream[A])(b: B)(f: (A, B) => B): B = as match{
            case Stream.Empty => b
            case x#::xs => foldRight(as)(f(x, b))(f)
        }
        def foldLeft[A, B](as: Stream[A])(b: B)(f: (B, A) => B): B = as match {
            case Stream.Empty =>  b
            case x#::xs => f(foldLeft(xs)(b)(f), x)
        }
        def foldMap[A, B <: Monoid[B]](as: Stream[A])(f: A => B)(b: B): B
            = foldRight(as.map(f))(b.mempty)(b.mappend)
    }
}

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object Tree{
    implicit class foldableTree[A](tree: Tree[A]) extends Foldable[Tree]{
        def foldRight[A, B](as: Tree[A])(b: B)(f: (A, B) => B): B = as match {
            case Leaf(a) => f(a, b) 
            case Branch(l, r) => foldRight(r)(foldRight(l)(b)(f))(f)
        }
        def foldLeft[A, B](as: Tree[A])(b: B)(f: (B, A) => B): B = as match {
            case Leaf(a) => f(b, a) 
            case Branch(l, r) => foldLeft(r)(foldLeft(l)(b)(f))(f)
        }
        def foldMap[A, B <: Monoid[B]](as: Tree[A])(f: A => B)(b: B): B 
            = foldRight(as)(b.mempty)((a, acc) => b.mappend(f(a), b))
    }
}

