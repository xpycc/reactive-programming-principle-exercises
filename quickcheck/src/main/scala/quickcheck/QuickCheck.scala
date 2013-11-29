package quickcheck

import common._
import scala.annotation._

import org.scalacheck._
import Arbitrary._
import Gen._
import Prop._
import Math._

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {

  property("min1") = forAll { a: Int =>
    val h = insert(a, empty)
    findMin(h) == a && isEmpty(deleteMin(h))
  }
  
  property("min2") = forAll {
    (a: Int, b: Int) =>
      val h = insert(b, insert(a, empty))
      findMin(h) == min(a, b) && findMin(deleteMin(h)) == max(a, b)
  }
  
  property("min3") = forAll {		// including commented test meld rank
    (a: Int, b: Int, c: Int) =>
      val h = List(a, b, c).foldRight(empty)(insert)
      val s = List(a, b, c).sorted
      findMin(deleteMin(h)) == s(1)
  }
  
  property("insert then delete") = forAll {
    a: Int =>
      val h = deleteMin(insert(a, empty))
      isEmpty(h)
  }

  property("check sorted") = forAll {
    h: H => {
    @tailrec
    def checkInc(prev: Int, h: H): Boolean = {
      if (isEmpty(h)) true
      else {
        val m = findMin(h)
        ord.lteq(prev, m) && checkInc(m, deleteMin(h)) 
      }
    }
      isEmpty(h) || checkInc(findMin(h), deleteMin(h))
    }
  }
  
  property("melding heaps") = forAll {
    (h: H, g: H) => {
//      println(h, g)
      val me = meld(h, g)
      isEmpty(me) || findMin(me) == (
        if (isEmpty(h)) findMin(g)
        else if (isEmpty(g)) findMin(h)
        else min(findMin(h), findMin(g))
      )
    }
  }
  
//  property("meld rank") = {
//    val h = List(1, 2).foldRight(empty)(insert)
//    val g = insert(4, h)
//    findMin(deleteMin(g)) == 2
//  }

  lazy val genHeap: Gen[H] = oneOf(empty,
    for {
      e <- arbitrary[Int]
      h <- oneOf(empty, genHeap)
    } yield insert(e, h)
  )

  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

}
