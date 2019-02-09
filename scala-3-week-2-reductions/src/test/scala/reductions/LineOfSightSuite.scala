package reductions

import java.util.concurrent._
import scala.collection._
import org.scalatest.FunSuite
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import common._
import java.util.concurrent.ForkJoinPool.ForkJoinWorkerThreadFactory

@RunWith(classOf[JUnitRunner])
class LineOfSightSuite extends FunSuite {

  import LineOfSight._


  test("single lineOfSight") {
    assert(4 === lineOfSight(3)(8, 2))
    assert(7 === lineOfSight(7)(8, 2))
    assert(3 === lineOfSight(2)(6, 2))
  }


  test("lineOfSight should correctly handle an array of size 4") {
    val output = new Array[Float](4)
    lineOfSight(Array[Float](0f, 1f, 8f, 9f), output)
    assert(output.toList == List(0f, 1f, 4f, 4f))
  }


  test("upsweepSequential empty") {
    val input = Array[Float]()
    assert(0f == upsweepSequential(input, 0, input length))
  }

  test("upsweepSequential should correctly handle the chunk 1 until 4 of an array of 4 elements") {
    val res = upsweepSequential(Array[Float](0f, 1f, 8f, 9f), 1, 4)
    assert(res == 4f)
  }


  test("upsweep empty") {

    val input = Array[Float]()

    assert(upsweep(input, 0, input length, 0) == Leaf(0, input length, 0))
    assert(upsweep(input, 0, input length, 1) == Leaf(0, input length, 0))
    assert(upsweep(input, 0, input length, 2) == Leaf(0, input length, 0))

  }

  test("upsweep [0, 2, 1, 5, 3, 2, 8, 7]") {

    val input = Array[Float](0f, 2f, 1f, 5f, 3f, 2f, 8f, 7f)
    val threshold = 4

    val left = Leaf(0, 4, 2f)
    val right = Leaf(4, 8, 4f/3f)

    val expected = Node(left, right)
    val actual = upsweep(input, 0, input length, threshold)

    assert(actual == expected)

    assert(expected.maxPrevious == upsweep(input, 0, input length, 1).maxPrevious)
    assert(expected.maxPrevious == upsweep(input, 0, input length, 2).maxPrevious)
    assert(expected.maxPrevious == upsweep(input, 0, input length, 3).maxPrevious)
    assert(expected.maxPrevious == upsweep(input, 0, input length, 6).maxPrevious)
    assert(expected.maxPrevious == upsweep(input, 0, input length, 7).maxPrevious)
    assert(expected.maxPrevious == upsweep(input, 0, input length, 8).maxPrevious)

  }


  test("downsweepSequential empty") {

    val input = Array[Float]()
    val output = Array[Float]()
    val other = Array[Float](1)

    downsweepSequential(input, output, 0f, 0, 0)

    assert(input === output)
    assert(input !== other)

  }

  test("downsweepSequential should correctly handle a 4 element array when the starting angle is zero") {
    val output = new Array[Float](4)
    downsweepSequential(Array[Float](0f, 1f, 8f, 9f), output, 0f, 1, 4)
    assert(output.toList == List(0f, 1f, 4f, 4f))
  }

}

