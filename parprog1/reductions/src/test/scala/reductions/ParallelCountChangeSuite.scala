package reductions

import java.util.concurrent._
import scala.collection._
import org.scalatest.FunSuite
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import common._

import ParallelCountChange._

@RunWith(classOf[JUnitRunner])
class ParallelCountChangeSuite extends FunSuite {

  test("countChange should return 0 for money < 0") {
    def check(money: Int, coins: List[Int]) = 
      assert(countChange(money, coins) == 0,
        s"countChang($money, _) should be 0")

    check(-1, List())
    check(-1, List(1, 2, 3))
    check(-Int.MinValue, List())
    check(-Int.MinValue, List(1, 2, 3))
  }

  test("countChange should return 1 when money == 0") {
    def check(coins: List[Int]) = 
      assert(countChange(0, coins) == 1,
        s"countChang(0, _) should be 1")

    check(List())
    check(List(1, 2, 3))
    check(List.range(1, 100))
  }

  test("countChange should return 0 for money > 0 and coins = List()") {
    def check(money: Int) = 
      assert(countChange(money, List()) == 0,
        s"countChang($money, List()) should be 0")

    check(1)
    check(Int.MaxValue)
  }

  test("countChange should work when there is only one coin") {
    def check(money: Int, coins: List[Int], expected: Int) =
      assert(countChange(money, coins) == expected,
        s"countChange($money, $coins) should be $expected")

    check(1, List(1), 1)
    check(2, List(1), 1)
    check(1, List(2), 0)
    check(Int.MaxValue, List(Int.MaxValue), 1)
    check(Int.MaxValue - 1, List(Int.MaxValue), 0)
  }

  test("countChange should work for multi-coins") {
    def check(money: Int, coins: List[Int], expected: Int) =
      assert(countChange(money, coins) == expected,
        s"countChange($money, $coins) should be $expected")

    check(50, List(1, 2, 5, 10), 341)
    check(250, List(1, 2, 5, 10, 20, 50), 177863)
  }

  test("parCountChange should return 0 for money < 0") {
    def check(money: Int, coins: List[Int]) =
      assert(parCountChange(money, coins, combinedThreshold(money, coins)) == 0,
        s"countChang($money, _) should be 0")

    check(-1, List())
    check(-1, List(1, 2, 3))
    check(-Int.MinValue, List())
    check(-Int.MinValue, List(1, 2, 3))
  }

  test("parCountChange should return 1 when money == 0") {
    def check(coins: List[Int]) =
      assert(parCountChange(0, coins, combinedThreshold(0, coins)) == 1,
        s"countChang(0, _) should be 1")

    check(List())
    check(List(1, 2, 3))
    check(List.range(1, 100))
  }

  test("parCountChange should return 0 for money > 0 and coins = List()") {
    def check(money: Int) =
      assert(parCountChange(money, List(), combinedThreshold(money, Nil)) == 0,
        s"countChang($money, List()) should be 0")

    check(1)
    check(Int.MaxValue)
  }

  test("parCountChange should work when there is only one coin") {
    def check(money: Int, coins: List[Int], expected: Int) =
      assert(parCountChange(money, coins, combinedThreshold(money, coins)) == expected,
        s"parCountChange($money, $coins) should be $expected")

    check(1, List(1), 1)
    check(2, List(1), 1)
    check(1, List(2), 0)
    check(Int.MaxValue, List(Int.MaxValue), 1)
    check(Int.MaxValue - 1, List(Int.MaxValue), 0)
  }

  test("parCountChange should work for multi-coins") {
    def check(money: Int, coins: List[Int], expected: Int) =
      assert(parCountChange(money, coins, combinedThreshold(money, coins)) == expected,
        s"parCountChange($money, $coins) should be $expected")

    check(50, List(1, 2, 5, 10), 341)
    check(250, List(1, 2, 5, 10, 20, 50), 177863)
  }

  // From FPP in Scala
  test("countChange: example given in instructions") {
    assert(countChange(4,List(1,2)) === 3)
  }

  test("countChange: sorted CHF") {
    assert(countChange(300,List(5,10,20,50,100,200,500)) === 1022)
  }

  test("countChange: no pennies") {
    assert(countChange(301,List(5,10,20,50,100,200,500)) === 0)
  }

  test("countChange: unsorted CHF") {
    assert(countChange(300,List(500,5,50,100,20,200,10)) === 1022)
  }

  // money
  test("parCountChange(moneyThreshold): example given in instructions") {
    assert(parCountChange(4,List(1,2),moneyThreshold(4)) === 3)
  }

  test("parCountChange(moneyThreshold): sorted CHF") {
    assert(parCountChange(300,List(5,10,20,50,100,200,500), moneyThreshold(300)) === 1022)
  }

  test("parCountChange(moneyThreshold): no pennies") {
    assert(parCountChange(301,List(5,10,20,50,100,200,500), moneyThreshold(301)) === 0)
  }

  test("parCountChange(moneyThreshold): unsorted CHF") {
    assert(parCountChange(300,List(500,5,50,100,20,200,10), moneyThreshold(300)) === 1022)
  }

  // totalCoins
  test("parCountChange(totalCoinsThreshold): example given in instructions") {
    assert(parCountChange(4,List(1,2),totalCoinsThreshold(2)) === 3)
  }

  test("parCountChange(totalCoinsThreshold): sorted CHF") {
    assert(parCountChange(300,List(5,10,20,50,100,200,500), totalCoinsThreshold(7)) === 1022)
  }

  test("parCountChange(totalCoinsThreshold): no pennies") {
    assert(parCountChange(301,List(5,10,20,50,100,200,500), totalCoinsThreshold(7)) === 0)
  }

  test("parCountChange(totalCoinsThreshold): unsorted CHF") {
    assert(parCountChange(300,List(500,5,50,100,20,200,10), totalCoinsThreshold(7)) === 1022)
  }

  // combined
  test("parCountChange(combinedThreshold): example given in instructions") {
    assert(parCountChange(4,List(1,2),combinedThreshold(4,List(1,2))) === 3)
  }

  test("parCountChange(combinedThreshold): sorted CHF") {
    assert(parCountChange(300,List(5,10,20,50,100,200,500), combinedThreshold(300,List(5,10,20,50,100,200,500))) === 1022)
  }

  test("parCountChange(combinedThreshold): no pennies") {
    assert(parCountChange(301,List(5,10,20,50,100,200,500), combinedThreshold(301,List(5,10,20,50,100,200,500))) === 0)
  }

  test("parCountChange(combinedThreshold): unsorted CHF") {
    assert(parCountChange(300,List(500,5,50,100,20,200,10), combinedThreshold(300,List(500,5,50,100,20,200,10))) === 1022)
  }
}
