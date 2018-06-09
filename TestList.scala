import scala.util.{Failure, Success, Try}

class Rational(x: Int, y: Int) {
  require(y > 0, "denominator must be positive")
  def this(x: Int) = this(x, 1) // forces denominator to be 1
  private def gcd(a: Int, b: Int): Int = {
    if (b == 0) a else gcd(b, a % b)
  }
  private val g = gcd(x, y)
  def numer = x / g
  def denom = y / g
}

abstract class IntSet {
  def incl(x: Int): IntSet
  def contains(x: Int): Boolean
}

class Empty extends IntSet {
  override def contains(x: Int): Boolean = false
  override def incl(x: Int): IntSet = new NonEmpty(x, new Empty, new Empty)
}

class NonEmpty(elem: Int, left: IntSet, right: IntSet) extends IntSet {
  override def contains(x: Int): Boolean =
    if (x < elem) left contains x
    else if (x > elem) right contains x
    else true

  override def incl(x: Int): IntSet =
    if (x < elem) new NonEmpty(elem, left incl x, right)
    else if (x > elem) new NonEmpty(elem, left, right incl x)
    else this
}

// since scala doesn't support multiple inheritance
// we have traits (which are like interfaces)

trait Planar {
  def height: Int
  def width: Int
  def surface: Int = height * width
}

// Example usage
// class Square extends Shape with Planar with Movable ... (we use "with" keyword with traits

abstract class Reducer(init: Int) {
  def combine(x: Int, y: Int): Int
  def reduce(xs: List[Int]): Int = {
    xs match {
      case Nil => init
      case y :: ys => combine(y, reduce(ys))
    }
  }
}

// with initial value 1
object Product extends Reducer(1) {
  def combine(x: Int, y: Int): Int = x * y
}

// with initial value 0
object Sum extends Reducer(0) {
  def combine(x: Int, y: Int): Int = x + y
}

// Allows us to be generic about types
abstract class Set[A] {
  def incl(a: A): Set[A]
  def contains(a: A): Boolean
}

object TestList {
  def insertionSort(xs: List[Int]): List[Int] = xs match {
    case List() => List()
    case y :: ys => insert(y, insertionSort(ys))
  }

  val cond: (Int, Int) => Boolean = (x, y) => x <= y

  def insert(x: Int, xs: List[Int]): List[Int] = {
    xs match {
      case List() => x :: Nil
      case y :: ys =>
        if (cond(x, y)) x :: y :: ys
        else y :: insert(x, ys)
    }
  }

  def sqrt(x: Double): Option[Double] = {
    if (x < 0) None else Some(Math.sqrt(x))
  }

  def foo(x: Double): String = {
    sqrt(x) match {
      case None => "No Result"
      case Some(y) => y.toString
    }
  }

  def bar(x: Double): Try[Double] = {
    if (x < 0) Failure(new IllegalArgumentException("x must be positive"))
    else Success(Math.sqrt(x))
  }

  // Either return the value on the left or on the right
  def baz(x: Double): Either[String, Double] = {
    if (x < 0) Left("x must be positive")
    else Right(Math.sqrt(x))
  }

  def average(x: Int, xs: Int*): Double =
    (x :: xs.to[List]).sum.toDouble / (xs.size + 1)

  def main(args: Array[String]): Unit = {
    // Similar to how Scheme does it
    val fruit = "apples" :: ("oranges" :: ("pears" :: Nil))

//    val nums = List(1, 2, 3, 4)
//    // decomposing a list using pattern matching
//    nums match {
//        // List that starts with 1 then 2
//      case 1 :: 2 :: xs => println(xs)
//        // List that has 1 element
//      case x :: Nil => println(x)
//        // List that has 1 elemnt
//      case List(x) => println("One element")
//        // The empty list
//      case List() => println("Empty list")
//        // rest of the elements after 2
//      case 2 :: xs => println(xs)
//    }

    val toSort: List[Int] = List(4, 3, 2, 1)
    val sortedList: List[Int] = insertionSort(toSort)
    println(sortedList)

    println(List(1, 2, 3).map(x => x + 1) == List(2, 3, 4))

    println(foo(4))

    // Tuple unpacking
    val is: (Int, String) = (42, "foo")

    val (i, s) = is
    println(i, s)

    val nums = List(1, 2, 3, 4)

    println(Product.reduce(nums))

    println(Sum.reduce(nums))

  }

}
