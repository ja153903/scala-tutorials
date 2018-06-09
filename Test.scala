import org.scalatest.Matchers._
import scala.annotation.tailrec

object Test {
  // One way we can define the concept of a parent class (without necessarily doing anything)
  // is through sealed traits
  sealed trait Symbol
  case class Note(name: String, duration: String, octave: Int) extends Symbol
  case class Rest(duration: String) extends Symbol

  // Since Symbol doesn't do anything, we might want to find out
  // whether the Symbol is a Note or a Rest
  def symbolDuration(symbol: Symbol): String = {
    symbol match {
      case Note(name, duration, octave) => duration
      case Rest(duration) => duration
    }
  }

//  def sumInts(a: Int, b: Int): Int =
//    if (a > b) 0 else a + sumInts(a + 1, b)

  def cube(x: Int): Int = x * x * x

//  def sumCubes(a: Int, b: Int): Int =
//    if (a > b) 0 else cube(a) + sumCubes(a + 1, b)

  def factorial(a: Int): Int =
    if (a == 1) 1 else a * factorial(a - 1)

//  def sumFactorials(a: Int, b: Int): Int =
//    if (a > b) 0 else factorial(a) + sumFactorials(a+1, b)

  // a more general version using higher-order functions
//  def sum(f: Int => Int, a: Int, b: Int): Int =
//    if (a > b) 0
//    else f(a) + sum(f, a+1, b)

  // tail-recursive version of sum
  def sum(f: Int => Int, a: Int, b: Int): Int = {
    @tailrec
    def loop(x: Int, acc: Int): Int = {
      if (x > b) acc
      else loop(x + 1, acc + f(x))
    }
    loop(a, 0)
  }

  def id(x: Int): Int = x
  def sumInts(a: Int, b:Int): Int = sum(x => x, a, b)
  def sumCubes(a: Int, b: Int): Int = sum(x => x*x*x, a, b)
  def sumFactorials(a: Int, b: Int): Int = sum(factorial, a, b)

  def main(args: Array[String]): Unit = {
    val c3 = Note("C", "Third", 3)
    c3.name shouldBe "C"
    c3.duration shouldBe "Third"
    c3.octave shouldBe 3

    // Almost like polymorphism, but no restraint on callables
    val symbol1: Symbol = Note("C", "Quarter", 2)
    val symbol2: Symbol = Rest("Whole")

    //println(sumInts(0, 4))

    println(sum(x => x, 1, 10))

  }
}

