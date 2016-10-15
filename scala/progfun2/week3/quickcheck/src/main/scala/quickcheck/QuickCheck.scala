package quickcheck

import common._

import org.scalacheck._
import Arbitrary._
import Gen._
import Prop._

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {

  lazy val genHeap: Gen[H] = for {
    v <- arbitrary[A]
    h <- frequency((1, Gen.const(empty)), (9, genHeap))
  } yield insert(v, h)
  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

  property("gen1") = forAll { (h: H) =>
    val m = if (isEmpty(h)) 0 else findMin(h)
    findMin(insert(m, h)) == m
  }

  property("hint2") = forAll { (a: A) =>
    isEmpty(deleteMin(insert(a, empty)))
  }

  property("hint1") = forAll { (n1: A, n2: A) =>
    val heap = insert(n2, insert(n1, empty))
    findMin(heap) == Math.min(n1, n2)
  }

  property("hint3") = forAll { (heap: H) =>
    def isSorted(heap: H): Boolean = {
      if (isEmpty(heap)) true
      else {
        val minimum = findMin(heap)
        val heap2 = deleteMin(heap)
        isEmpty(heap2) || (minimum <= findMin(heap2) && isSorted(heap2))
      }
    }
      isSorted(heap)
  }

  property("hint4") = forAll { (heap1: H, heap2: H) =>
    findMin(meld(heap1, heap2)) == Math.min(findMin(heap1), findMin(heap2))
  }

  property("equality") = forAll { (heap1: H, heap2: H) =>
      def equals(heap1: H, heap2: H): Boolean = {
        if (isEmpty(heap1) && isEmpty(heap2)) true
        else findMin(heap1) == findMin(heap2) && equals(deleteMin(heap1), deleteMin(heap2))
      }

      equals(meld(heap1, heap2), meld(deleteMin(heap1), insert(findMin(heap1), heap2)))
  }

}
