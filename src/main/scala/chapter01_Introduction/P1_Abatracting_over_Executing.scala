package chapter01_Introduction

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.{ExecutionContext, Future}
import scala.io.StdIn

object P1_Abatracting_over_Executing {

  trait TerminalSync {
    def read(): String

    def write(t: String): Unit
  }

  trait TerminalAsync {
    def read(): Future[String]

    def write(t: String): Future[Unit]
  }

  trait Foo[C[_]] {
    def create(i: Int): C[Int]
  }

  object FooList extends Foo[List] {
    def create(i: Int): List[Int] = List(i)
  }

  object FooEitherString extends Foo[Either[String, ?]] {
    def create(i: Int): Either[String, Int] = Right(i)
  }

  //  type Id[T] = T
  object FooId extends Foo[λ[A => A]] {
    def create(i: Int): Int = i
  }

  trait Terminal[C[_]] {
    def read: C[String]

    def write(t: String): C[Unit]
  }

  object Terminal {

    implicit object TerminalSync extends Terminal[λ[X => X]] {
      def read: String = ???

      def write(t: String): Unit = ???
    }

    implicit object TerminalAsync extends Terminal[Future] {
      def read: Future[String] = ???

      def write(t: String): Future[Unit] = ???
    }

    implicit object TerminalIO extends Terminal[IO] {
      def read: IO[String] = IO {
        println("请输入您所需要的字符串: ")
        StdIn.readLine
      }

      def write(t: String): IO[Unit] = IO {
        println(s"您的数据为:  $t")
      }
    }

  }

  //  type Now[X] = X

  trait Execution[C[_]] {
    def doAndThen[A, B](c: C[A])(f: A => C[B]): C[B]

    def flatMap2[A, B](a: C[A])(f: A => C[B]): C[B] = doAndThen(a)(f) // 与上面的等价

    def create[B](b: B): C[B]

    def pure[A](a: A): C[A] = create(a) // 与上面的模式等价
  }

  //  def echo[C[_]](t: Terminal[C], e: Execution[C]): C[String] =
  //    e.chain(t.read) { in: String =>
  //      e.chain(t.write(in)) { _: Unit =>
  //        e.create(in)
  //      }
  //    }

  object Execution {

    implicit class Ops[A, C[_]](c: C[A]) {
      def flatMap[B](f: A => C[B])(implicit e: Execution[C]): C[B] =
        e.doAndThen(c)(f)

      def map[B](f: A => B)(implicit e: Execution[C]): C[B] = // 用flatmap来使用map的机制!!!
        e.doAndThen(c)(f andThen e.create)
    }

    implicit val now: Execution[λ[X => X]] = new Execution[λ[X => X]] {
      def doAndThen[A, B](c: A)(f: A => B): B = f(c)

      def create[B](b: B): B = b
    }

    //    implicit def future(implicit EC: ExecutionContext): Execution[Future] =
    implicit def future: Execution[Future] =
      new Execution[Future] {
        def doAndThen[A, B](c: Future[A])(f: A => Future[B]): Future[B] =
          c.flatMap(f)

        def create[B](b: B): Future[B] = Future.successful(b)
      }

    implicit val deferred: Execution[IO] = new Execution[IO] {
      def doAndThen[A, B](c: IO[A])(f: A => IO[B]): IO[B] = c.flatMap(f)

      def create[B](b: B): IO[B] = IO(b)
    }
  }

  final class IO[A](val interpret: () => A) {
    def map[B](f: A => B): IO[B] = IO(f(interpret()))   // 直接生成的IO逻辑,

    def flatMap[B](f: A => IO[B]): IO[B] = IO(f(interpret()).interpret())  // 之前的逻辑先跑完,然后再跑这个逻辑
  }

  object IO {
    // 看来这个是懒加载,thunk代码没有立即执行,所以需要我们手动去触发计算逻辑
    def apply[A](a: => A): IO[A] = new IO(() => a)
  }

  //  object TerminalIO extends Terminal[IO] {
  //    def read: IO[String]           = IO { io.StdIn.readLine }
  //    def write(t: String): IO[Unit] = IO { println(t) }
  //  }

  import Execution._

  //  def echo[C[_]](implicit t: Terminal[C], e: Execution[C]): C[String] =
  //    t.read.flatMap { in: String =>
  //      t.write(in).map { _: Unit =>
  //        in
  //      }
  //    }

//  def echo[C[_]](implicit t: Terminal[C], e: Execution[C]): C[String] =
//    for {
//      in <- t.read
//      _ <- t.write(in)
//    } yield in

  def echo[C[_]](implicit t: Terminal[C], e: Execution[C]): C[String] =
    (t.read).flatMap(in => t.write(in).map(_ => in))


  def main(args: Array[String]): Unit = {
    import Execution._
    import Terminal._
    val delayed: IO[String] = echo[IO]

    delayed.interpret()

    //      val futureEcho: Future[String] = echo[Future](futureEcho, ??? )
    /**
     * We have broken purity and are no longer writing FP code: futureEcho is the result of
     * running echo once. Future conflates the definition of a program with interpreting it
     * (running it). As a result, applications built with Future are difficult to reason about.
     *
     * An expression is referentially transparent if it can be replaced with its corresponding
     * value without changing the program’s behaviour.
     *
     * Pure functions are referentially transparent, allowing for a great deal of code reuse,
     * performance optimisation, understanding, and control of a program.
     *
     * Impure functions are not referentially transparent. We cannot replace echo[Future] with
     * a value, such as val futureEcho, since the pesky user can type something different the
     * second time.
     */

  }
}
