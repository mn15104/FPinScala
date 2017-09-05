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