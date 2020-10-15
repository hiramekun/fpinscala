package chap2

object Ex2_2 extends App {
  println(isSorted(Array(1, 2, 3), (a: Int, b: Int) => a < b))
  println(isSorted(Array(1, 4, 3), (a: Int, b: Int) => a < b))

  def isSorted[A](as: Array[A], ordered: (A, A) => Boolean): Boolean = {
    @annotation.tailrec
    def loop(as: Array[A], ordered: (A, A) => Boolean, idx: Int): Boolean = {
      if (idx >= as.length - 1) true
      else if (!ordered(as(idx), as(idx + 1))) false
      else loop(as, ordered, idx + 1)
    }

    loop(as, ordered, 0)
  }
}
