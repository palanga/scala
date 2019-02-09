package reductions

import org.scalameter._
import common._

final class ChainingOps[A](private val self: A) extends AnyVal {
  def pipe[B](f: A => B): B = f(self)

  def |>[B](f: A => B): B = pipe(f)
}

object LineOfSightRunner {
  
  val standardConfig = config(
    Key.exec.minWarmupRuns -> 40,
    Key.exec.maxWarmupRuns -> 80,
    Key.exec.benchRuns -> 100,
    Key.verbose -> true
  ) withWarmer(new Warmer.Default)

  def main(args: Array[String]) {
    val length = 10000000
    val input = (0 until length).map(_ % 100 * 1.0f).toArray
    val output = new Array[Float](length + 1)
    val seqtime = standardConfig measure {
      LineOfSight.lineOfSight(input, output)
    }
    println(s"sequential time: $seqtime ms")

    val partime = standardConfig measure {
      LineOfSight.parLineOfSight(input, output, 10000)
    }
    println(s"parallel time: $partime ms")
    println(s"speedup: ${seqtime / partime}")
  }
}

object LineOfSight {

  implicit class Pipe[A](a: A) {
    def |>[B](f: A => B): B = f(a)
  }

  def max(a: Float, b: Float): Float = if (a > b) a else b

  def curriedMax(a: Float)(b: Float): Float = if (a > b) a else b

  def tangent(o: Float, a: Float): Float = o / a

  def curriedTangent(o: Float)(a: Float): Float = o / a

  def lineOfSight(prevMax: Float)(o: Float, a: Float): Float = tangent(o, a) |> curriedMax(prevMax)

  def lineOfSight(input: Array[Float], output: Array[Float]): Unit = {
    def scan(prevMax: Float, currentIndex: Int): Unit = {
      if (currentIndex < input.length) {
        output.update(currentIndex, lineOfSight(prevMax)(input(currentIndex), currentIndex))
        scan(max(prevMax, output(currentIndex)), currentIndex + 1)
      }
    }

    output.update(0, 0f)
    scan(0, 1)
  }

  sealed abstract class Tree {
    def maxPrevious: Float
  }

  case class Node(left: Tree, right: Tree) extends Tree {
    val maxPrevious: Float = max(left.maxPrevious, right.maxPrevious)
  }

  case class Leaf(from: Int, until: Int, maxPrevious: Float) extends Tree

  /** Traverses the specified part of the array and returns the maximum angle.
   */
  def upsweepSequential(input: Array[Float], from: Int, until: Int): Float = {
    def reduce(prevMax: Float, currentIndex: Int): Float = {
      if (currentIndex == until) prevMax
      else reduce(lineOfSight(prevMax)(input(currentIndex), currentIndex), currentIndex + 1)
    }

    if (from == until) 0f
    else if (from == 0) reduce(0f, 1)
    else reduce(0f, from)
  }

  /** Traverses the part of the array starting at `from` and until `end`, and
   *  returns the reduction tree for that part of the array.
   *
   *  The reduction tree is a `Leaf` if the length of the specified part of the
   *  array is smaller or equal to `threshold`, and a `Node` otherwise.
   *  If the specified part of the array is longer than `threshold`, then the
   *  work is divided and done recursively in parallel.
   */
  def upsweep(input: Array[Float], from: Int, end: Int,
    threshold: Int): Tree = {
    ???
  }

  /** Traverses the part of the `input` array starting at `from` and until
   *  `until`, and computes the maximum angle for each entry of the output array,
   *  given the `startingAngle`.
   */
  def downsweepSequential(input: Array[Float], output: Array[Float],
    startingAngle: Float, from: Int, until: Int): Unit = {
    ???
  }

  /** Pushes the maximum angle in the prefix of the array to each leaf of the
   *  reduction `tree` in parallel, and then calls `downsweepSequential` to write
   *  the `output` angles.
   */
  def downsweep(input: Array[Float], output: Array[Float], startingAngle: Float,
    tree: Tree): Unit = {
    ???
  }

  /** Compute the line-of-sight in parallel. */
  def parLineOfSight(input: Array[Float], output: Array[Float],
    threshold: Int): Unit = {
    ???
  }
}
