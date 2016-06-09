package reductions

import java.util.concurrent._
import scala.collection._
import org.scalatest.FunSuite
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import common._

import ParallelParenthesesBalancing._

@RunWith(classOf[JUnitRunner])
class ParallelParenthesesBalancingSuite extends FunSuite {

  test("balance should work for empty string") {
    def check(input: String, expected: Boolean) =
      assert(balance(input.toArray) == expected,
        s"balance($input) should be $expected")

    check("", true)
  }

  test("balance should work for string of length 1") {
    def check(input: String, expected: Boolean) =
      assert(balance(input.toArray) == expected,
        s"balance($input) should be $expected")

    check("(", false)
    check(")", false)
    check(".", true)
  }

  test("balance should work for string of length 2") {
    def check(input: String, expected: Boolean) =
      assert(balance(input.toArray) == expected,
        s"balance($input) should be $expected")

    check("()", true)
    check(")(", false)
    check("((", false)
    check("))", false)
    check(".)", false)
    check(".(", false)
    check("(.", false)
    check(").", false)
  }

  // sequential
  test("balance: '(if (zero? x) max (/ 1 x))' is balanced") {
    assert(balance("(if (zero? x) max (/ 1 x))".toCharArray))
  }

  test("balance: 'I told him ...' is balanced") {
    assert(balance("I told him (that it's not (yet) done).\n(But he wasn't listening)".toCharArray))
  }

  test("balance: ':-)' is unbalanced") {
    assert(!balance(":-)".toCharArray))
  }

  test("balance: counting is not enough") {
    assert(!balance("())(".toCharArray))
  }

  // parallel
  test("parBalance: '(if (zero? x) max (/ 1 x))' is balanced") {
    assert(parBalance("(if (zero? x) max (/ 1 x))".toCharArray, 2))
  }

  test("parBalance: 'I told him ...' is parBalanced") {
    assert(parBalance("I told him (that it's not (yet) done).\n(But he wasn't listening)".toCharArray, 2))
  }

  test("parBalance: ':-)' is unBalanced") {
    assert(!parBalance(":-)".toCharArray, 2))
  }

  test("parBalance: counting is not enough") {
    assert(!parBalance("())(".toCharArray, 2))
  }

  test("parBalance: should work for string of length 2 and threshold 1") {
    assert(parBalance("()".toCharArray, 1))
  }
}