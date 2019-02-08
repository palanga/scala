package reductions

import org.junit.runner.RunWith
import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner
import reductions.ParallelParenthesesBalancing._

@RunWith(classOf[JUnitRunner])
class SequentialParenthesesBalancingSuite extends FunSuite {

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

}

@RunWith(classOf[JUnitRunner])
class ParallelParenthesesBalancingSuite extends FunSuite {


  test("reduceChunkOptimized should work for empty string") {
    def check(input: String, expected: (Int, Int)) =
      assert(reduceChunkOptimized(input toArray, 0, input length) == expected,
        s"reduceChunkOptimized($input) should be $expected")

    check("", (0, 0))
  }

  test("reduceChunkOptimized should work for string of length 1") {
    def check(input: String, expected: (Int, Int)) =
      assert(reduceChunkOptimized(input toArray, 0, input length) == expected,
        s"reduceChunkOptimized($input) should be $expected")

    check("a", (0, 0))
    check("(", (1, 0))
    check(")", (-1, -1))

  }

  test("reduceChunkOptimized should work for string of length 2") {
    def check(input: String, from: Int, to: Int, expected: (Int, Int)) =
      assert(reduceChunkOptimized(input toArray, from, to) == expected,
        s"reduceChunkOptimized($input, $from, $to) should be $expected")

    check("ab", 0, 2, (0, 0))
    check("ab", 0, 1, (0, 0))
    check("ab", 1, 2, (0, 0))
    check("(b", 0, 2, (1, 0))
    check("(b", 0, 1, (1, 0))
    check("(b", 1, 2, (0, 0))
    check(")b", 0, 2, (-1, -1))
    check(")b", 0, 1, (-1, -1))
    check(")b", 1, 2, (0, 0))
    check("a(", 0, 2, (1, 0))
    check("a(", 0, 1, (0, 0))
    check("a(", 1, 2, (1, 0))
    check("a)", 0, 2, (-1, -1))
    check("a)", 0, 1, (0, -0))
    check("a)", 1, 2, (-1, -1))
    check("()", 0, 2, (0, 0))
    check("()", 0, 1, (1, 0))
    check("()", 1, 2, (-1, -1))
    check(")(", 0, 2, (0, -1))
    check(")(", 0, 1, (-1, -1))
    check(")(", 1, 2, (1, 0))

  }


  test("reduceOptimized should work for empty string") {
    def check(input: String, threshold: Int, expected: (Int, Int)) =
      assert(reduceOptimized(input toArray, threshold, 0, input length) == expected,
        s"reduceOptimized($input, $threshold) should be $expected")

    check("", 0, (0, 0))
    check("", 1, (0, 0))
  }

  test("reduceOptimized should work for string of length 1") {
    def check(input: String, threshold: Int, expected: (Int, Int)) =
      assert(reduceOptimized(input toArray, threshold, 0, input length) == expected,
        s"reduceOptimized($input, $threshold) should be $expected")

    check("a", 1, (0, 0))
    check("(", 1, (1, 0))
    check(")", 1, (-1, -1))
  }

  test("reduceOptimized should work for string of length > 1") {
    def check(input: String, threshold: Int, expected: (Int, Int)) =
      assert(reduceOptimized(input toArray, threshold, 0, input length) == expected,
        s"reduceOptimized($input, $threshold) should be $expected")

    check("ab", 2, (0, 0))
    check("(b", 2, (1, 0))
    check(")b", 2, (-1, -1))
    check("a(", 2, (1, 0))
    check("a)", 2, (-1, -1))
    check("()", 2, (0, 0))
    check(")(", 2, (0, -1))

    check("ab", 1, (0, 0))
    check("(b", 1, (1, 0))
    check(")b", 1, (-1, -1))
    check("a(", 1, (1, 0))
    check("a)", 1, (-1, -1))
    check("()", 1, (0, 0))
    check(")(", 1, (0, -1))

    check("a()(e)gh", 2, (0, 0))
    check("ab)(ef()", 2, (0, -1))
    check("a()(efg)", 2, (0, 0))
    check("a(((e)))", 2, (0, 0))

    check("ab)(ef()", 4, (0, -1))
    check("a))(e)((", 4, (0, -2))
    check("a(((e)))", 4, (0, 0))

  }


  test("parBalance should work for empty string") {
    def check(input: String, threshold: Int, expected: Boolean) =
      assert(parBalance(input toArray, threshold) == expected,
        s"parBalance($input, $threshold) should be $expected")

    check("", 0, true)
  }

  test("parBalance should work for string of length 1") {
    def check(input: String, threshold: Int, expected: Boolean) =
      assert(parBalance(input toArray, threshold) == expected,
        s"parBalance($input, $threshold) should be $expected")

    check("a", 1, true)
    check("(", 1, false)
    check(")", 1, false)
  }

  test("parBalance should work for string of length > 1") {
    def check(input: String, threshold: Int, expected: Boolean) =
      assert(parBalance(input toArray, threshold) == expected,
        s"parBalance($input, $threshold) should be $expected")

    check("ab", 1, true)
    check("(b", 1, false)
    check(")b", 1, false)
    check("a(", 1, false)
    check("a)", 1, false)
    check("()", 1, true)
    check(")(", 1, false)

    check("ab", 2, true)
    check("(b", 2, false)
    check(")b", 2, false)
    check("a(", 2, false)
    check("a)", 2, false)
    check("()", 2, true)
    check(")(", 2, false)

    check("a()(e)gh", 2, true)
    check("ab)(ef()", 2, false)
    check("a()(efg)", 2, true)
    check("a(((e)))", 2, true)

    check("ab)(ef()", 4, false)
    check("a))(e)((", 4, false)
    check("a(((e)))", 4, true)

  }

}
