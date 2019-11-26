package chapter02_For_Comprehensions
import scala.reflect.runtime.universe._
object P1_Synatax_Sugar {


  def main(args: Array[String]): Unit = {
    val a, b, c = Option(1)
    val r = show { reify {
      for { i <- a ; j <- b ; k <- c } yield i + j + k
    } }

    println(s"r is $r")

    val r2 = show { reify {
      for {
        i <- a
        j <- b
        ij = i + j
        k <- c
      } yield ij + k
    }}

    println(s"r2 is $r2")




    val r3 = show { reify {
       for {
         i <- a
         j <- b
         if i > j
         k <- c
       } yield i + j + k
    }}

    println(s"r3 is $r3")

    val init = 4
    val r4 = show { reify{ init + 4}}
    println(s"r4 is $r4")

    val r5 = show { reify {
      for {
        i <- a
        j <- b
      } println(s"$i, $j")
    }}
    println(s"r5 is $r5")

    /**
     * It often surprises developers when inline Future calculations in a for
     * comprehension do not run in parallel
     *
     * This is because the flatMap spawning anotherExpensiveCalc is strictly after expensiveCalc.
     */
    import scala.concurrent._
    import ExecutionContext.Implicits.global
    def expensiveCalc(): Int = ???
    def anotherExpensiveCalc(): Int = ???
    for {
      i <- Future { expensiveCalc() }
      j <- Future { anotherExpensiveCalc() }
    } yield (i + j)


    /**
     * To ensure that two Future calculations begin in parallel, start them outside the
     * for comprehension.
     */

    val aa = Future { expensiveCalc() }
    val bb = Future { anotherExpensiveCalc() }
    for { i <- aa ; j <- bb } yield (i + j)
  }
}
