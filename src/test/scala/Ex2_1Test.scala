import chap2.Ex2_1._
import org.scalatest.flatspec._
import org.scalatest.matchers._

class Ex2_1Test extends AnyFlatSpec with should.Matchers {
  "fib" should "フィボナッチ数列の計算ができる" in {
    fib(0) should be(0)
    fib(1) should be(1)
    fib(2) should be(1)
    fib(3) should be(2)
    fib(5) should be(5)
  }
}
