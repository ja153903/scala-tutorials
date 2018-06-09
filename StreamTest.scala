object StreamTest {
  def streamRange(lo: Int, hi: Int): Stream[Int] =
    if (lo >= hi) Stream.empty
    else Stream.cons(lo, streamRange(lo + 1, hi))

  def isPrime(x: Int): Boolean =
    if (x < 2) false else !((2 until x - 1) exists (x % _ == 0))

  def main(args: Array[String]): Unit = {
    val xs = Stream.cons(1, Stream.cons(2, Stream.empty))

    println((streamRange(1000, 10000) filter isPrime)(0))
  }
}
