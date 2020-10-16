import datastructures.List
import datastructures.List._
import org.scalatest.flatspec._
import org.scalatest.matchers._

class ListTest extends AnyFlatSpec with should.Matchers {
  "sum" should "Listの合計を計算する" in {
    sum(List(1, 2, 3)) should be(6)
    sum(List(1)) should be(1)
    sum(List()) should be(0)
  }

  "setHead" should "最初の要素を置き換える" in {
    setHead(List(1, 2, 3), 3) should be(List(3, 2, 3))
    setHead(List(), 3) should be(List())
  }

  "drop" should "指定した個数の要素を頭から削除したリストを返す" in {
    drop(List(1, 2, 3), 2) should be(List(3))
    drop(List(), 2) should be(List())
  }

  "dropWhile" should "述語を満たす限り要素を削除する" in {
    dropWhile(List("a", "aa", "aaa", "a", "aaa"), (str: String) => str.length < 3) should be(List("aaa", "a", "aaa"))
  }

  "init" should "末尾の要素を除く全ての要素で構成したリストを返す" in {
    init(List(1, 2, 3)) should be(List(1, 2))
  }
}
