package funsets


/**
  * 2. Purely Functional Sets.
  */
object FunSets {
  /**
    * We represent a set by its characteristic function, i.e.
    * its `contains` predicate.
    */
  type Set = Int => Boolean

  /**
    * Indicates whether a set contains a given element.
    */
  def contains(s: Set, elem: Int): Boolean = s(elem)

  /**
    * Returns the set of the one given element.
    */
  def singletonSet(elem: Int): Set = (x: Int) => elem == x


  /**
    * Returns the union of the two given sets,
    * the sets of all elements that are in either `s` or `t`.
    */
  def union(s: Set, t: Set): Set = (x: Int) => contains(s, x) || contains(t, x)

  /**
    * Returns the intersection of the two given sets,
    * the set of all elements that are both in `s` and `t`.
    */
  def intersect(s: Set, t: Set): Set = x => contains(s, x) && contains(t, x)

  /**
    * Returns the difference of the two given sets,
    * the set of all elements of `s` that are not in `t`.
    */
  def diff(s: Set, t: Set): Set = x => contains(s, x) && !contains(t, x)

  /**
    * Returns the subset of `s` for which `p` holds.
    */
  def filter(s: Set, p: Int => Boolean): Set = x => p(x) && contains(s, x)


  /**
    * The bounds for `forall` and `exists` are +/- 1000.
    */
  val bound = 1000

  /**
    * Returns whether all bounded integers within `s` satisfy `p`.
    */
  def forall(s: Set, p: Int => Boolean): Boolean = {
    def iter(a: Int): Boolean = {
      if (a > bound) true
      else if (contains(s, a) && !p(a)) false
      else iter(a + 1)
    }

    iter(-bound)
  }

  /**
    * Returns whether there exists a bounded integer within `s`
    * that satisfies `p`.
    */
  def exists(s: Set, p: Int => Boolean): Boolean = !forall(s, !p(_))

  /**
    * Returns a set transformed by applying `f` to each element of `s`.
    */
  def map(s: Set, f: Int => Int): Set = x => exists(s,  x == f(_))

  /**
    * Displays the contents of a set
    */
  def toString(s: Set): String = {
    val xs = for (i <- -bound to bound if contains(s, i)) yield i
    xs.mkString("{", ",", "}")
  }

  /**
    * Prints the contents of a set on the console.
    */
  def printSet(s: Set) {
    println(toString(s))
  }
}


//Your overall score for this assignment is 9.00 out of 10.00
//
//
//The code you submitted did not pass all of our tests: your submission achieved a score of
//7.00 out of 8.00 in our tests.
//
//In order to find bugs in your code, we advise to perform the following steps:
//- Take a close look at the test output that you can find below: it should point you to
//the part of your code that has bugs.
//- Run the tests that we provide with the handout on your code.
//- The tests we provide do not test your code in depth: they are very incomplete. In order
//to test more aspects of your code, write your own unit tests.
//- Take another very careful look at the assignment description. Try to find out if you
//misunderstood parts of it. While reading through the assignment, write more tests.
//
//Below you can find a short feedback for every individual test that failed.
//
//Our automated style checker tool could not find any issues with your code. You obtained the maximal
//style score of 2.00.
//
//======== LOG OF FAILED TESTS ========
//Your solution achieved a testing score of 210 out of 240.
//
//Below you can see a short feedback for every test that failed,
//indicating the reason for the test failure and how many points
//you lost for each individual test.
//
//Tests that were aborted took too long too complete or crashed the
//JVM. Such crashes can arise due to infinite non-terminating
//loops or recursion (StackOverflowException) or excessive memory
//consumption (OutOfMemoryException).
//
//[Test Description] map: {1,3,4,5,7,1000}
//[Observed Error] "{[2,4,5,6,8]}" did not equal "{[0,2,3,4,6,999]}"
//[Lost Points] 10
//
//[Test Description] forall & map: doubling numbers
//[Observed Error] FunSets.forall($anon.this.doubled, ((x$14: Int) => x$14.%(2).==(0))) was false The set obtained by doubling all numbers should contain only even numbers.
//[Lost Points] 10
//
//[Test Description] exists should be implemented in terms of forall
//[Observed Error] InstrumentedSuite.this.wasCalled(call) was false expected forall to be called
//[Lost Points] 10
//
//======== TESTING ENVIRONMENT ========
//Limits: memory: 256m,  total time: 850s,  per test case time: 240s