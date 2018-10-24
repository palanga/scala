package quickcheck

import common._

import org.scalacheck._
import Arbitrary._
import Gen._
import Prop._

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {

  lazy val genHeap: Gen[H] = oneOf(
    const(empty),
    for {
      n <- arbitrary[Int]
      h <- oneOf(const(empty), genHeap)
    } yield insert(n, h)
  )

  lazy val singleHeap: Gen[H] = for {
    n <- arbitrary[Int]
  } yield insert(n, empty)

  lazy val nonEmptyHeap: Gen[H] = oneOf(
    singleHeap,
    for {
      n <- arbitrary[Int]
      h <- oneOf(singleHeap, genHeap)
    } yield insert(n, h)
  )

  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

  property("min1") = forAll { a: Int =>
    val h = insert(a, empty)
    findMin(h) == a
  }

  property("gen1") = forAll { (h: H) =>
    val m = if (isEmpty(h)) 0 else findMin(h)
    findMin(insert(m, h)) == m
  }

  property("minimum of two") = forAll { (n: Int, m: Int) =>
    val heap1 = insert(n, empty)
    val heap2 = insert(m, heap1)
    val min = Math.min(n, m)
    findMin(heap2) == min
  }

  property("delete minimum") = forAll { n: Int =>
    val heap = insert(n, empty)
    deleteMin(heap) == empty
  }

  property("delete minimum of two") = forAll { (n: Int, m: Int) =>
    val heap1 = insert(n, empty)
    val heap2 = insert(m, heap1)
    val heapWithMax = deleteMin(heap2)
    val max = Math.max(n, m)
    findMin(heapWithMax) == max
  }

  property("sorted") = forAll { h: H => isSorted(h) }

  property("sorted from list") = forAll { list: List[Int] =>
    val h = insertAll(list, empty)
    isSorted(h)
  }

  def isSorted(heap: H): Boolean = {
    if (isEmpty(heap)) true
    else if (isEmpty(deleteMin(heap))) true
    else if (findMin(heap) > findMin(deleteMin(heap))) false
    else isSorted(deleteMin(heap))
  }

  def toList(acc: List[Int], heap: H): List[Int] = {
    if (isEmpty(heap)) acc
    else toList(findMin(heap) :: acc, deleteMin(heap))
  }

  def insertAll(list: List[Int], h: H): H = list.foldLeft(empty)((h, n) => insert(n, h))

  property("insert all -> to list") = forAll { list: List[Int] =>
    val h = insertAll(list, empty)
    val l = toList(Nil, h).reverse
    list.sorted == l
  }

  property("minimum of meld") = forAll(nonEmptyHeap, nonEmptyHeap) { (h1: H, h2: H) =>
    val min1 = findMin(h1)
    val min2 = findMin(h2)
    val melt = meld(h1, h2)
    val meltMin = findMin(melt)
    meltMin == min1 || meltMin == min2
  }

}
