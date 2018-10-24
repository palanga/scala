package funsets

import org.scalatest.FunSuite


import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

/**
  * This class is a test suite for the methods in object FunSets. To run
  * the test suite, you can either:
  *  - run the "test" command in the SBT console
  *  - right-click the file in eclipse and chose "Run As" - "JUnit Test"
  */
@RunWith(classOf[JUnitRunner])
class FunSetSuite extends FunSuite {

  /**
    * Link to the scaladoc - very clear and detailed tutorial of FunSuite
    *
    * http://doc.scalatest.org/1.9.1/index.html#org.scalatest.FunSuite
    *
    * Operators
    *  - test
    *  - ignore
    *  - pending
    */

  /**
    * Tests are written using the "test" operator and the "assert" method.
    */
  test("string take") {
    val message = "hello, world"
    assert(message.take(5) == "hello")
  }

  /**
    * For ScalaTest tests, there exists a special equality operator "===" that
    * can be used inside "assert". If the assertion fails, the two values will
    * be printed in the error message. Otherwise, when using "==", the test
    * error message will only say "assertion failed", without showing the values.
    *
    * Try it out! Change the values so that the assertion fails, and look at the
    * error message.
    */
  test("adding ints") {
    assert(1 + 2 === 3)
  }


  import FunSets._

  test("contains is implemented") {
    assert(contains(_ => true, 100))
  }

  test("{ x => x > 0 } contains 1") {
    assert(contains(x => x > 0, 1))
  }

  test("{ x => x < 0 } not contains 1") {
    assert(!contains(x => x < 0, 1))
  }

  /**
    * When writing tests, one would often like to re-use certain values for multiple
    * tests. For instance, we would like to create an Int-set and have multiple test
    * about it.
    *
    * Instead of copy-pasting the code for creating the set into every test, we can
    * store it in the test class using a val:
    *
    * val s1 = singletonSet(1)
    *
    * However, what happens if the method "singletonSet" has a bug and crashes? Then
    * the test methods are not even executed, because creating an instance of the
    * test class fails!
    *
    * Therefore, we put the shared values into a separate trait (traits are like
    * abstract classes), and create an instance inside each test method.
    *
    */

  trait TestSets {
    val s1: Set = singletonSet(1)
    val s2: Set = singletonSet(2)
    val s3: Set = singletonSet(3)
    val positives: Set = _ > 0
    val negatives: Set = _ < 0
    val moduloTwo: Set = _ % 2 == 0
    val moduloThree: Set = _ % 3 == 0
    val moduloTwelve: Set = _ % 12 == 0
    val all: Set = _ => true
    val oneTwoThree: Set = x => x == 1 || x == 2 || x == 3
  }

  /**
    * This test is currently disabled (by using "ignore") because the method
    * "singletonSet" is not yet implemented and the test would fail.
    *
    * Once you finish your implementation of "singletonSet", exchange the
    * function "ignore" by "test".
    */
  test("singletonSet(1) contains 1") {

    /**
      * We create a new instance of the "TestSets" trait, this gives us access
      * to the values "s1" to "s3".
      */
    new TestSets {
      /**
        * The string argument of "assert" is a message that is printed in case
        * the test fails. This helps identifying which assertion failed.
        */
      assert(contains(s1, 1), "Singleton")
    }
  }

  test("union contains all elements of each set") {
    new TestSets {
      val s: Set = union(s1, s2)
      assert(contains(s, 1), "Union 1")
      assert(contains(s, 2), "Union 2")
      assert(!contains(s, 3), "Union 3")
    }
  }

  test("intersect contains only the elements each set have in common") {
    new TestSets {
      val empty: Set = intersect(positives, negatives)
      val moduloSix: Set = intersect(moduloTwo, moduloThree)
      assert(!contains(empty, -1), "empty not contains -1")
      assert(!contains(empty, 0), "empty not contains 0")
      assert(!contains(empty, 1), "empty not contains 1")
      assert(contains(moduloSix, 0), "multiplesOfSix contains 0")
      assert(contains(moduloSix, 6), "multiplesOfSix contains 6")
      assert(contains(moduloSix, 12), "multiplesOfSix contains 12")
      assert(!contains(moduloSix, 2), "multiplesOfSix not contains 2")
      assert(!contains(moduloSix, 3), "multiplesOfSix not contains 3")
      assert(!contains(moduloSix, 4), "multiplesOfSix not contains 4")
      assert(!contains(moduloSix, 5), "multiplesOfSix not contains 5")
      assert(!contains(moduloSix, 7), "multiplesOfSix not contains 7")
    }
  }

  test("diff contains only the elements each set not have in common") {
    new TestSets {
      val positivesAndZero: Set = diff(all, negatives)
      val odds: Set = diff(all, moduloTwo)

      assert(!contains(positivesAndZero, -2), "positivesAndZero not contains -2")
      assert(!contains(positivesAndZero, -1), "positivesAndZero not contains -1")
      assert(contains(positivesAndZero, 0), "positivesAndZero contains 0")
      assert(contains(positivesAndZero, 1), "positivesAndZero contains 1")
      assert(contains(positivesAndZero, 2), "positivesAndZero contains 2")

      assert(contains(odds, -1), "odds contains -1")
      assert(contains(odds, 1), "odds contains 1")
      assert(contains(odds, 3), "odds contains 3")
      assert(!contains(odds, -4), "odds not contains -4")
      assert(!contains(odds, -2), "odds not contains -2")
      assert(!contains(odds, 0), "odds not contains 0")
      assert(!contains(odds, 2), "odds not contains 2")
      assert(!contains(odds, 4), "odds not contains 4")
    }
  }

  test("filtered set contains only the elements the set has and pass the filter function") {
    new TestSets {
      val empty: Set = filter(moduloTwo, _ % 2 == 1)
      val greaterThanSeven: Set = filter(all, _ >= 7)

      assert(!contains(greaterThanSeven, -2), "greaterThanSeven not contains -2")
      assert(!contains(greaterThanSeven, 0), "greaterThanSeven not contains 0")
      assert(!contains(greaterThanSeven, 6), "greaterThanSeven not contains 6")
      assert(contains(greaterThanSeven, 7), "greaterThanSeven contains 7")
      assert(contains(greaterThanSeven, 11), "greaterThanSeven contains 11")
      assert(contains(greaterThanSeven, 13), "greaterThanSeven contains 13")

      assert(!contains(empty, -2), "empty not contains -2")
      assert(!contains(empty, -1), "empty not contains -1")
      assert(!contains(empty, 0), "empty not contains 0")
      assert(!contains(empty, 1), "empty not contains 1")
      assert(!contains(empty, 2), "empty not contains 2")
    }
  }

  test("forall") {
    new TestSets {
      assert(forall(s2, _ == 2))
      assert(forall(moduloTwo, _ % 2 == 0))
      assert(forall(moduloTwo, _ % 2 != 1))
      assert(forall(moduloTwelve, x => contains(moduloTwo, x)))
      assert(forall(moduloTwelve, x => contains(moduloThree, x)))
    }
  }

  test("exists") {
    new TestSets {
      assert(exists(s2, _ == 2))
      assert(!exists(s2, _ == 1))
      assert(exists(moduloTwo, _ % 2 == 0))
      assert(!exists(moduloTwo, _ % 2 != 0))
      assert(exists(moduloTwelve, x => contains(moduloTwo, x)))
      assert(exists(moduloTwelve, x => contains(moduloThree, x)))
    }
  }

  test("map") {
    new TestSets {
      val twoThreeFour: Set = map(oneTwoThree, _ + 1)

      printSet(oneTwoThree)
      printSet(twoThreeFour)

      assert(!contains(twoThreeFour, 1))
      assert(contains(twoThreeFour, 2))
      assert(contains(twoThreeFour, 3))
      assert(contains(twoThreeFour, 4))
    }
  }

}
