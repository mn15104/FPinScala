import Stream._

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object FunctionalDataStructures {
  def abs(n: Int): Int =
    if (n < 0) -n
    else n
  private def formatAbs(x: Int) = {
    val msg = "The absolute value of %d is %d"
    msg.format(x, abs(x))
  }

  //Exercise 1
  def fib(n: Int): Int = {
    if (n == 1 | n == 0)
      return 1
    else
      return n + fib(n-1) + fib(n-2)
  }

  def formatfib(n: Int) = {
    val msg = "The fib of %d is %d."
    msg.format(n, fib(n))
  }

  def format_func(name: String, n: Int, f: Int => Int) = {
    val msg = "The %s of %d is %d"
    msg.format(name, n, f(n))

  }

  //Exercise 2
  def isSorted[A](as: List[A], lessThan: (A, A) => Boolean ) : Boolean = {
      var i = 0
      for(i <- 0 to as.size -1 ){
        if (lessThan(as(i+1), as(i)))
            false
      }
      true
  }

  //Exercise 3
  def partial1[A, B, C](a: A, f: (A,B) => C): B => C = {
      (b: B) => f(a,b)
  }

  //Exercise 4
  def curry[A, B, C](f: (A, B) => C) : A => (B => C) = {
    (a: A) => ((b: B) => f(a, b))
  }

  //Exercise 5
  def uncurry[A, B, C](f: A => (B => C)) : (A, B) => C = {
    (a: A, b: B) => f(a)(b)
  }

  //Exercise 6
  def compose[A,B,C](f: B => C, g: A => B) : A => C = {
    (a: A) => f(g(a))
  }

  //CHAPTER 3 - FUNCTIONAL DATA STRUCTURES
  // PART 1: LISTS

  //Exercise 2
  def tail[A](as: List[A]) = as match {
      case Nil => Nil
      case ::(x, xs)  => xs
    }

  //Exercise 3
  def drop[A](as: List[A], n: Int) : List[A] = as match {
      case Nil => Nil
      case ::(x, xs) => if (n > 1) drop(xs, n-1) else xs
  }

  //Exercise 4
  def dropWhile[A](as: List[A], f: A => Boolean) : List[A] = as match {
      case Nil => Nil
      case ::(x, xs) => if (f(x)) dropWhile(xs, f) else as
  }
  def dropWhile2[A](as: List[A], f: A => Boolean) : List[A] = as match {
      case Nil => Nil
      case ::(x, xs) => if (f(x)) dropWhile(xs, f) else as
  }

  val isEven = (x: Int) => {if ((x % 2) == 0) true else false}

  //Exercise 5
  def setHead[A](as: List[A], a: A) : List[A] = as match {
      case Nil => ::(a, Nil)
      case ::(x, xs) => ::(a, xs)
  }

  //Exercise 6
  def init[A](as: List[A]) : List[A] = as match {
      case Nil => Nil
      case ::(x, xs) => if (xs == Nil) Nil else ::(x, init(xs))
  }

  //Exercise 7
  def productFoldr(as: List[Int]) : Int = {
    def prod_foldr(as: List[Int], z: Int) : Int = as match{
      case Nil => z
      case ::(x, xs)  => if (x == 0) 0 else prod_foldr(xs, x * z)
    }
    prod_foldr(as, 1)
  }
  //Exercise 8
  def foldr_exercise8() = {
    val x = foldRight(List(1,2,3), Nil:List[Int])(::(_,_))
  }

  //Exercise 9
  def foldr_length[A](as: List[A]) : Int = {
    foldRight(as, 0)((x:A, z: Int) => z + 1)
  }

  //Exercise 10
  def foldLeft[A,B](l: List[A], z: B)(f: (B, A) => B) : B = l match {
    case Nil => z
    case ::(x, xs) => foldLeft(xs, f (z, x)) (f)
  }
  def foldRight[A,B](l: List[A], z: B)(f: (A, B) => B): B = l match {
    case Nil => z
    case ::(x, xs) =>  f(x, foldRight(xs, z)(f))
  }
  //Exercise 12 - We cannot pass only 'Nil' to foldLeft as Scala's type inference needs to know the list type
  def fold_reverse[A](l: List[A]) : List[A] = {
    foldl_foldr(l, Nil : List[A]) ((a, b) => ::(b, a))
  }

  //Exercise 13
  def foldl_foldr[A,B](l: List[A], z: B)(f: (B, A) => B) : B = {
    foldRight (l, (identity[B] _)) {  (a, b) => (acc: B) => b(f(acc, a)) } (z)
  }
  //Exercise 14
  def foldr_append[A, B](l: List[A], z: A) : List[A] = {
    val empty = List[A]()
    //foldLeft (l, empty) { (b: B, a: A) => {if (b == Nil) ::(a, List(z)) else (::(a,b))}
    foldRight(l, empty) {(x:A, xs:List[A]) => if (xs == Nil) ::(x, List(z)) else ::(x, xs)}

  }
  //Exercise 16
  def foldl_inc_all(l: List[Int]) = {
    val empty = List[Int]()
    foldRight(l, empty) {(x:Int, a:List[Int]) => ::(x+1, a)}
    foldLeft(l, empty){(a:List[Int], x:Int) => (a ::: List(x+1))}
  }

  //Exercise 18
  def map[A,B](l:List[A])(f: A => B) : List[B] = {
    val empty = List[B]()
    foldRight(l, empty) {(x: A, bs:List[B]) => ::(f (x), bs)}
  }

  //Exercise 19
  def filter[A](l:List[A])(f: A => Boolean) : List[A] = l match{
    case Nil => Nil
    case ::(x, xs) => if (f(x) == true) ::(x, filter(xs)(f)) else filter(xs)(f)
  }

  //Exercise 20
  def flatmap[A,B](l:List[A])(f:A => List[B]) : List[B] = l match{
    case Nil => Nil
    case ::(x, xs) => {println(f(x)); (f(x)) ::: flatmap(xs)(f)}
  }

  //Exercise 21
  def flatmap_filter[A, B](l:List[A])(f:A => Boolean) = {
    flatmap(l){x => if (f(x) == true) List(x) else Nil}
  }

  //Exercise 23
  def zipWith[A, B](l1 : List[A], l2 : List[A])(f: (A, A) => B) : List[B] = (l1, l2) match {
    case (Nil, _) => Nil
    case (_, Nil) => Nil
    case (::(x, xs), ::(y, ys)) => ::(f(x,y), zipWith(xs, ys)(f))
  }

  //Exercise 25
  def tree_size(tree: Tree[Int]) : Int = tree match {
    case Leaf(_) => 1
    case Branch(l, r) => tree_size(l) + tree_size(r)
  }
  //Exericse 26
  def tree_max(tree: Tree[Int]) : Int = tree match {
    case Leaf(x) => x
    case Branch(l, r) => if (tree_max(l) > tree_max(r)) tree_max(l) else tree_max(r)
  }
  //Exercise 27
  def tree_depth[A](tree: Tree[A]) : Int = tree match {
    case Leaf(x) => 1
    case Branch(l, r) => if (tree_depth(l) + 1 > tree_depth(r) + 1) tree_depth(l) + 1 else tree_depth(r) + 1
  }
  //Exercise 28
  def tree_map[A,B](tree: Tree[A])(f: A => B) : Tree[B] = tree match {
    case Leaf(x) => Leaf(f(x))
    case Branch(l, r) => Branch(tree_map(l)(f), tree_map(r)(f))
  }
  //Exercise 29
  // def tree_fold[A,B](z: B, tree: Tree[A])(f: (A, B) => B) : B = tree match {
  //   case Leaf(x) => f(x, z)
  //   case Branch(l, r) => tree_fold(z, l)(f))
  // }
}
import java.util.regex._
//Exercise 1
sealed trait Maybe[+A]{
  def map[B](f: A => B): Maybe[B] = this match {
      case Nothing => Nothing
      case Just(a) => Just(f(a))
  }
  def flatMap[B](f: A => Maybe[B]): Maybe[B] = this match {
    case Nothing => Nothing
    case Just(a) => f(a)
  }
  def getOrElse[B >: A](default: => B): B = this match {
    case Nothing => default
    case Just(a) => a
  }
  def orElse[B>:A](ob: => Maybe[B]): Maybe[B] =
     map (Just(_)) getOrElse ob             //wraps our 'Just(a)' or 'Nothing' to become 'Just(Just(a))' or 'Just(Nothing)'
                                            //for getOrElse() to peek inside it
  def filter(f: A => Boolean): Maybe[A] = this match {
    case Just(a) if f(a) => this
    case _ => Nothing
  }
}
case class Just[+A](get: A) extends Maybe[A]
case object Nothing extends Maybe[Nothing]
    
object Main{
  
  def pattern(s: String): Maybe[Pattern] =
    try {
      Just(Pattern.compile(s))
    } catch {
      case e: PatternSyntaxException => Nothing
  }

  //Lifting with map
  def lift[A,B](f: A => B): Maybe[A] => Maybe[B] = _ map f 

  def mkMatcher(pat: String): Maybe[String => Boolean] =
    pattern(pat) map (p => (s: String) => p.matcher(s).matches)

  //Lifting with for-comprehension
  def mkMatcher_1(pat: String): Maybe[String => Boolean] =
  for {
    p <- pattern(pat)
  } yield ((s: String) => p.matcher(s).matches)
  // pattern(pat) map (p => (s: String) => p.matcher(s).matches)

  def doesMatch(pat: String, s: String): Maybe[Boolean] =
  for {
    p <- mkMatcher_1(pat)
  } yield p(s)
  // => mkMatcher_1(pat) map (p => p(s))
  // :: Maybe[String => Boolean] map ((String => Boolean) => Boolean)

  def bothMatch(pat: String, pat2: String, s: String): Maybe[Boolean] =
  for {
    f <- mkMatcher(pat)
    g <- mkMatcher(pat2)
  } yield f(s) && g(s)

  def bothMatch_1(pat: String, pat2: String, s: String): Maybe[Boolean] =
      mkMatcher(pat) flatMap (f =>
      mkMatcher(pat2) map     (g =>
      f(s) && g(s)))
  def mean(xs: Seq[Double]): Maybe[Double] = {
    if (xs.isEmpty) Nothing else Just(xs.sum/xs.length)
  }
  //Exercise 2
  def variance(xs: Seq[Double]): Maybe[Double] = {
    if (xs.isEmpty) Nothing
    else { Just(((xs.map(a => scala.math.pow(a - xs.sum/xs.length, 2) )).sum)/xs.length) }
  }
  //Exercise 3
  def map2[A,B,C](a: Maybe[A], b: Maybe[B])(f: (A, B) => C): Maybe[C] =
    a flatMap (a_ => b map (b_ => f(a_, b_)))

  //Exercise 4
  def bothMatch_2(pat1: String, pat2: String, s: String): Maybe[Boolean] =
    map2(mkMatcher(pat1), mkMatcher(pat2))((a,b) => a(s) && b(s))
  //Exercise 5
  def sequence[A](al: List[Maybe[A]]): Maybe[List[A]] = {
    Just(al.flatMap((am: Maybe[A]) => am match {
      case Nothing => (Nil) 
      case Just(a) => (List(a)) 
    })).flatMap(ls => if (ls.size < al.size) Nothing else Just(ls))
  }
  def sequence1[A](al: List[Maybe[A]]): Maybe[List[A]] = al match{
    case Nil => Just(Nil)
    case ::(x, xs) => x flatMap (a => sequence1(xs) map (xs_m => xs_m ::: List(a) ))
  }
  //Excercise 6
  def parsePatterns(a:List[String]): Maybe[List[Pattern]] = 
    sequence(a map pattern)
  
  def traverse[A, B](as: List[A])(f: A => Maybe[B]): Maybe[List[B]] = as match{
    case Nil => Just(Nil)
    case ::(x, xs) => 
      (f(x) flatMap (b => traverse(xs)(f) map (xs_m => xs_m ::: List(b))) ) 
  }
}











//
