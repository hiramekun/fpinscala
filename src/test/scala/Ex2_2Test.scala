import chap2.Ex2_2._
import org.scalatest.flatspec._
import org.scalatest.matchers._

class Ex2_2Test extends AnyFlatSpec with should.Matchers {
  "isSorted" should "ソートされた配列を正しく判別できる" in {
    val smaller = (a: Int, b: Int) => a < b
    isSorted(Array(1, 2, 3), smaller) should be (true)
    isSorted(Array(1, 4, 3), smaller) should be (false)
  }
}
