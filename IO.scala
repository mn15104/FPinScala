import scala.language.implicitConversions
import scala.language.higherKinds


// abstract case class Pure[F[_], +A](get: A) extends IO[F, A]
// abstract case class Request[F[_], I, +A](
//     expr: F[I],
//     receive: I => IO[F,A]) extends IO[F,A]
object IO1{
    trait IO[+A] { self =>
        def run: A 
    }
    object IO{
        implicit def ioMonad1[A] = new Monad[IO] {
            def flatMap[A, B](a: IO[A])(g: A => IO[B]): IO[B] =
                new IO[B] { def run = g(a.run).run }
            def unit[A](a: => A): IO[A] = 
                new IO[A] { def run = a}
        }
        implicit def ioMonad2[A](io: IO[A]) = new Monad[IO] {
            def flatMap[A, B](a: IO[A])(g: A => IO[B]): IO[B] =
                new IO[B] { def run = g(a.run).run }
            def unit[A](a: => A): IO[A] = 
                new IO[A] { def run = a}
        }
        def apply[A](a: => A)(implicit io: Monad[IO]): IO[A] = io.unit(a)
    }
}
object IO2{
    trait IO[F[_], +A] { self =>
        def run: A 
    }
    abstract case class Pure[F[_], +A](get: A) extends IO[F, A]
    abstract case class Request[F[_], I, +A](
        expr: F[I],
        receive: I => IO[F,A]) extends IO[F,A]
    object IO{
        def monad[F[_]] = new Monad[({ type f[A] = IO[F,A]})#f] {
            def flatMap[A, B](a: IO[F,A])(g: A => IO[F, B]): IO[F, B] =
                new IO[F, B] { def run = g(a.run).run }
            def unit[A](a: => A): IO[F, A] = 
                new IO[F, A] { def run = a}
        }
    }
    trait Console[A]
    case object ReadLine extends Console[Option[String]]
    case class PrintLine(s: String) extends Console[Unit]
}
object Main extends App{
   
}
// trait Console[A]
// case object ReadLine extends Console[Option[String]]
// case class PrintLine(s: String) extends Console[Unit]

// trait Run[F[_]] {
//   def apply[A](expr: F[A]): (A, Run[F])
// }

// object RunConsoleMock extends Run[Console] {
//   def apply[A](c: Console[A]) = c match {
//     case ReadLine => (Some("Hello world!"), RunConsoleMock)
//     case PrintLine(_) => ((), RunConsoleMock)
//   }
// }
// object RunConsole extends Run[Console] {
//   def apply[A](c: Console[A]) = c match {
//     case ReadLine =>
//       val r = try Some(readLine) catch { case _ => None }
//       (r, RunConsole)
//     case PrintLine(s) => (println(s), RunConsole)
//   }
// }
