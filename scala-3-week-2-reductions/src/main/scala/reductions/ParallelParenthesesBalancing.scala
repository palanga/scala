package reductions

import java.lang.Math.min

import common._
import org.scalameter._

import scala.annotation.tailrec

object ParallelParenthesesBalancingRunner {

  @volatile var seqResult = false

  @volatile var parResult = false

  val standardConfig = config(
    Key.exec.minWarmupRuns -> 40,
    Key.exec.maxWarmupRuns -> 80,
    Key.exec.benchRuns -> 120,
    Key.verbose -> true
  ) withWarmer(new Warmer.Default)

  def main(args: Array[String]): Unit = {
    val length = 100000000
    val chars = new Array[Char](length)
    val threshold = 10000
    val seqtime = standardConfig measure {
      seqResult = ParallelParenthesesBalancing.balance(chars)
    }
    println(s"sequential result = $seqResult")
    println(s"sequential balancing time: $seqtime ms")

    val fjtime = standardConfig measure {
      parResult = ParallelParenthesesBalancing.parBalance(chars, threshold)
    }
    println(s"parallel result = $parResult")
    println(s"parallel balancing time: $fjtime ms")
    println(s"speedup: ${seqtime / fjtime}")
  }
}

object ParallelParenthesesBalancing {

  /** Returns `true` iff the parentheses in the input `chars` are balanced.
    */
  def balance(chars: Array[Char]): Boolean = {

    @tailrec
    def reduce(acc: Int, chars: List[Char]): Boolean = chars match {
      case Nil => true
      case _ if acc < 0 => false
      case _ :: Nil => acc == 0
      case '(' :: tail => reduce(acc + 1, tail)
      case ')' :: tail => reduce(acc - 1, tail)
      case _ :: tail => reduce(acc, tail)
    }

    @tailrec
    def reduceOptimized(acc: Int, i: Int): Boolean = {
      if (i == chars.length) acc == 0
      else chars.charAt(i) match {
        case _ if acc < 0 => false
        case '(' => reduceOptimized(acc + 1, i + 1)
        case ')' => reduceOptimized(acc - 1, i + 1)
        case _ => reduceOptimized(acc, i + 1)
      }
    }

    reduceOptimized(0, 0)

  }

  def reduceNotOptimized(chars: Array[Char], threshold: Int): (Int, Int) = {
    if (chars.length <= threshold || threshold == 0) reduceChunk(chars)
    else {
      val (left, right) = chars.splitAt(chars.length / 2)
      parallel(reduceNotOptimized(left, threshold), reduceNotOptimized(right, threshold)) match {
        case ((prevSum, prevMin), (sum, min)) => (prevSum + sum, Math.min(prevMin, min + prevSum))
      }
    }
  }

  def reduceChunk(chars: Array[Char]): (Int, Int) = chars
    .foldLeft((0, 0)) {
      case ((sum, min), '(') => (sum + 1, min)
      case ((sum, min), ')') => (sum - 1, if (sum <= 0) min - 1 else min)
      case ((sum, min), _) => (sum, min)
    }

  def reduceChunkOptimized(chars: Array[Char], from: Int, to: Int): (Int, Int) = {

    @tailrec
    def reduce(acc: (Int, Int), currentIndex: Int): (Int, Int) = {
      if (currentIndex == to) acc
      else (acc, chars.charAt(currentIndex)) match {
        case ((sum, min), '(') => reduce((sum + 1, min), currentIndex + 1)
        case ((sum, min), ')') => reduce((sum - 1, if (sum <= 0) min - 1 else min), currentIndex + 1)
        case ((sum, min), _) => reduce((sum, min), currentIndex + 1)
      }
    }

    @tailrec
    def reduceOptimized(sum: Int, min: Int, currentIndex: Int): (Int, Int) = {
      if (currentIndex == to) (sum, min)
      else chars.charAt(currentIndex) match {
        case '(' => reduceOptimized(sum + 1, min, currentIndex + 1)
        case ')' => reduceOptimized(sum - 1, if (sum <= 0) min - 1 else min, currentIndex + 1)
        case _ => reduceOptimized(sum, min, currentIndex + 1)
      }
    }

    reduceOptimized(0, 0, from)

  }

  /**
    * Associativity law: a op (b op c) === (a op b) op c
    *
    * (aSum, aMin) op ((bSum, bMin) op (cSum, cMin) ) ==>
    * (aSum, aMin) op (bSum + cSum, bMin min (bSum + cMin)) ==>
    * (aSum + bSum + cSum, aMin min (aSum + (bMin min (bSum + cMin))))
    *
    * and
    *
    * ((aSum, aMin) op (bSum, bMin)) op (cSum, cMin) ==>
    * (aSum + bSum, aMin min (aSum + bMin)) op (cSum, cMin) ==>
    * (aSum + bSum + cSum, (aMin min (aSum + bMin)) min (aSum + bSum + cMin))
    *
    * from the first coordinate
    *
    * aSum + bSum + cSum === aSum + bSum + cSum
    *
    * so, we have to prove that
    *
    * aMin min (aSum + (bMin min (bSum + cMin))) === (aMin min (aSum + bMin)) min (aSum + bSum + cMin) ==>
    * aMin min ((aSum + bMin) min (aSum + bSum + cMin)) === (aMin min (aSum + bMin)) min (aSum + bSum + cMin)
    *
    * let's call
    *
    * x := aMin
    * y := aSum + bMin
    * z := aSum + bSum + cMin
    *
    * after that, we have
    *
    * x min (y min z) === (x min y) min z
    *
    * and that is the associativity of the __min__ function, which is true,
    * therefore, we have proven the associativity of our reduction operation.
    *
    */
  def reduceOptimized(chars: Array[Char], threshold: Int, from: Int, to: Int): (Int, Int) = {

    if (to - from <= threshold || threshold == 0) reduceChunkOptimized(chars, from, to)
    else {
      val splitPoint = from + (to - from) / 2
      parallel(
        reduceOptimized(chars, threshold, from, splitPoint),
        reduceOptimized(chars, threshold, splitPoint, to)
      ) match {
        case ((leftSum, leftMin), (rightSum, rightMin)) => (leftSum + rightSum, min(leftMin, leftSum + rightMin))
      }
    }
  }

  /** Returns `true` iff the parentheses in the input `chars` are balanced.
    */
  def parBalance(chars: Array[Char], threshold: Int): Boolean = {

    reduceOptimized(chars, threshold, 0, chars length) == (0, 0)

  }

  // For those who want more:
  // Prove that your reduction operator is associative!

}
