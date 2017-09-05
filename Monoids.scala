// Type Class
trait Monoid[A] {
    def mappend(a1: A, a2: A): A 
    def mempty: A 
    def mconcat(as: List[A], a: Monoid[A]): A = 
        as.foldRight(a.mempty)((bm, am) => a.mappend(bm, am))
    def foldMap[A,B](as: List[A], m: Monoid[B])(f:A => B): B =
        as.foldRight(m.mempty)((a:A, b:B) => m.mappend(b, f(a)))
}

sealed trait WC
case class Stub(chars: String) extends WC
case class Part(lStub: String, words: Int, rStub: String) extends WC

object WC {
    implicit val wcMonoid: Monoid[WC] = new Monoid[WC]{
        def mappend(a: WC, b: WC): WC = (a,b) match {
            case (Part(l1, w1, r1), Part(l2,w2,r2)) =>
                if(r1.isEmpty && l2.isEmpty) 
                    Part(l1 ++ r1 ++ l2, w2+w1, r2)
                else 
                    Part(l1 ++ r1 ++ l2, w2+w1+1, r2)
            case (Stub(s), Part(l2,w2,r2))      => Part(s+l2, w2, r2)
            case (Part(l1, w1, r1), Stub(s))    => Part(l1, w1, r1++s)
            case (Stub(s1), Stub(s2))           => Part(s1++s2, 0, "")
        }
        def mempty: WC = Stub("")
    }
    def countWords(s: String) = {
        val split = s.split(" ")
        val words = if (s(s.length-1) == " " && s(0) == " ") 
        Part(split(0), split.length - 2, split(split.length-1))
    }
    
    def reccountWords(s:String) : WC = {
        val str = Stub(s)
        wcMonoid.mappend(countWords(s.substring(0, s.length()/2)), countWords(s.substring(s.length()/2)))
    }
}

object Main extends App{
    println(WC.countWords("hi there man "))
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