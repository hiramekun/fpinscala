package chap2

object Ex2_1 extends App {
  for (i <- 0 to 10) {
    println(Ex2_1.fib(i))
  }

  def fib(n: Int): Int = {
    @annotation.tailrec
    def go(n: Int, sum: Int, now: Int): Int =
      if (n == 0) {
        sum
      } else {
        go(n - 1, sum + now, sum)
      }

    go(n, 0, 1)
  }
}
