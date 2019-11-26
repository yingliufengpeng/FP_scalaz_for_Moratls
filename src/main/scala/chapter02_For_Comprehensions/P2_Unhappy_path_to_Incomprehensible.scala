package chapter02_For_Comprehensions
import scala.concurrent.duration.Duration
import scala.reflect.runtime.universe._
object P2_Unhappy_path_to_Incomprehensible {

  def main(args: Array[String]): Unit = {
//    val a, b, c = Option(1)
//
//    def namedThingsOption(
//                     someName  : Option[String],
//                     someNumber: Option[Int]
//                   ): Option[String] = for {
//      name   <- someName
//      number <- someNumber
//    } yield s"$number ${name}s"
//
//    def namedThings(name: String, num: Int) = s"$num ${name}s"

    val a = Right(1)
    val b = Right(2)
    val c: Either[String, Int] = Left("sorry, no c ")

    import scala.concurrent._
    import ExecutionContext.Implicits.global

    val r = for {
      i <- Future.failed[Int](new Throwable)
      j <- Future { println("hello") ; 1 }
    } yield (i + j)

    val r2 = Await.result(r, Duration.Inf)
    println(s"r2 is $r2")

    def getFromRedis(s: String): Future[Option[String]] = ???
    def getFromSql(s: String): Future[Option[String]] = ???

    def key = ""
    val r3 = for {
      cache <- getFromRedis(key)
      sql   <- getFromSql(key)
    } yield cache orElse sql

    for {
      cache <- getFromRedis(key)
      res   <- cache match {
        case Some(_) => Future.successful(cache)
        case None    => getFromSql(key)
      }
    } yield res

    def getA: Future[Int] = ???
    def error(msg: String): Future[Nothing] =
      Future.failed(new RuntimeException(msg))

    val r4 = for {
      a <- getA
      b <- if (a <= 0) error(s"$a must be positive")
      else Future.successful(a)
    } yield b * 10


    def getB: Future[Int] = ???

    val r5 = for {
      a <- getA
      c <- if (a <= 0) Future.successful(0)
      else for { b <- getB } yield a * b
    } yield c

    import scalaz._, Scalaz._
    import simulacrum._
    for {
      a <- getA
      c <- if (a <= 0) 0.pure[Future]
      else for { b <- getB } yield a * b
    } yield c

    for {
      a <- getA
      b <- getB
    } yield a * b

    def getA2: Future[Option[Int]] = ???
    def getB2: Future[Option[Int]] = ???

//    for {
//      a <- getA2
//      b <- getB2
//    } yield a * b

    import scalaz.std
//    val result = for {
//      a <- OptionT(getA)
//      b <- OptionT(getB)
//    } yield a * b

    def getC: Future[Int] = ???

    /**
     * he monad transformer also allows us to mix Future[Option[_]] calls with methods that just
     * return plain Future via .liftM[OptionT] (provided by scalaz):
     */
    //    val result2 = for {
//      a <- OptionT(getA)
//      b <- OptionT(getB)
//      c <- getC.liftM[OptionT]
//    } yield a * b / c

    /**
     * and we can mix with methods that return plain Option by wrapping
     * them in Future.successful (.pure[Future]) followed by OptionT
     */
    def getD: Option[Int] = ???

//    val result3 = for {
//      a <- OptionT(getA)
//      b <- OptionT(getB)
//      c <- getC.liftM[OptionT]
//      d <- OptionT(getD.pure[Future])
//    } yield (a * b) / (c * d)

    /**
     *
     * It is messy again, but it is better than writing nested flatMap and map by
     * hand. We can clean it up with a DSL that handles all the required conversions
     * into OptionT[Future, _] combined with the |> operator, which applies the function
     * on the right to the value on the left, to visually separate the logic from the transformers
     */
    def liftFutureOption[A](f: Future[Option[A]]): OptionT[Future, A] = OptionT(f)
    def liftFuture[A](f: Future[A]): OptionT[Future, A] = f.liftM[OptionT]
    def liftOption[A](o: Option[A]): OptionT[Future, A] = OptionT(o.pure[Future])
    def lift[A](a: A): OptionT[Future, A] = liftOption(Option(a))

    val result4 = for {
      a <- getA2       |> liftFutureOption    // 等价于 a <- (getA2 |> liftFutureOption)
      b <- getB2       |> liftFutureOption
      c <- getC        |> liftFuture
      d <- getD        |> liftOption
      e <- 10          |> lift
    } yield e * (a * b) / (c * d)
  }
}
