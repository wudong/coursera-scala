package quickcheck

import common._
import org.scalacheck._
import Arbitrary._
import Gen._
import Prop.{BooleanOperators, forAll}

import scala.annotation.tailrec

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {

  lazy val genList : Gen[List[Int]] = Gen.listOf(Gen.posNum[Int])

  lazy val genHeap: Gen[H] = {
    val genHeap1 = for {
      v <- Gen.posNum[Int]
      m <- oneOf(const(empty), genHeap)
    } yield insert(v, m)

    val m = for {
      h1 <-genHeap1
      h2 <- genHeap1
    } yield meld(h1, h2)

    oneOf(const(empty), m)
  }


  def listToHeap(list: List[Int]) = list.foldLeft(empty)((h,int)=> insert(int, h))

  def heapToList(seed: H) = {
    @tailrec
    def deleteElementToList(heap: H, list: List[Int]) : List[Int] = {
      if (isEmpty(heap)){
        list
      }else{
        deleteElementToList(deleteMin(heap), findMin(heap):: list )
      }
    }

    deleteElementToList(seed, List()).reverse
  }


  def isOrdered(list: List[Int]) : Boolean = {

    @tailrec
    def checkOrderLoop(curElement: Int, next: Int, currentList : List[Int]): Boolean = {
      if (currentList == Nil) true
      else {
        if (!ord.lteq(curElement, next)) false
        else {
          checkOrderLoop(next, currentList.head, currentList.tail)
        }
      }
    }

    list match {
      case Nil  => true
      case ele :: Nil => true
      case a :: b :: rest => checkOrderLoop(a, b, rest)
    }
  }

  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

  property("gen1") = forAll { (h: H) =>
    val m = if (isEmpty(h)) 0 else findMin(h)
    findMin(insert(m, h)) == m
  }

  property("min1") = forAll { a: Int =>
    val h = insert(a, empty)
    findMin(h) == a
  }

  property("min2") = forAll {
    (a : Int, b: Int) =>{
      val h1 = insert(a, empty)
      val h2 = insert(b, h1)
      val h3 = deleteMin(h2)
      findMin(h3) == (a max b)
    }
  }

  property("sorting with deletion") = forAll {
    (a: H) => {
      if (isEmpty(a)) true
      else {
        val list = heapToList(a)
        isOrdered(list)
      }
    }
  }

  property("insert a list") = forAll {
    (list: List[Int]) => {
      list match {
        case Nil => true
        case _ => {
          findMin(listToHeap(list))==list.min
        }
      }
    }
  }

  property("merge one emtpy") = forAll {
    (a: Int)=>isEmpty(meld(empty, empty))
  }

  property("find min, delete min and add back") = forAll {
    (a: H)=> {
      if (isEmpty(a)) true
      else {
        val min = findMin(a)
        min == findMin(insert(min, deleteMin(a)))
      }
    }
  }

  property("merge one emtpy") = forAll {
    (a: H)=> {
      if (isEmpty(a)) {
        isEmpty(meld(a, empty))
      }else{
        val minA = findMin(a)
        findMin(meld(a, empty)) == minA
      }
    }
  }

  property("merge two non empty ") = forAll {
    (a: H, b: H)=> {
      (!isEmpty(a) && !isEmpty(b)) ==> {
        val minA =findMin(a)
        val minB =findMin(b)
        (minA min minB) == findMin(meld(a, b))
      }
    }
  }

  property("insert and remove") = forAll {
    (ele: Int) => {
      isEmpty(deleteMin(insert(ele, empty)))
    }
  }

  property("delete min") = forAll {
    (h: H) => {
      if (isEmpty(h)) true
      else{
        val newHeap =deleteMin(h)
        if (isEmpty(newHeap)) true
        else findMin(h) <= findMin(newHeap)
      }
    }
  }

  property("delete to get empty") = forAll {
    (i: Int) => {
      isEmpty(deleteMin(insert(i, empty)))
    }
  }


  property("sorting with corrected") = forAll {
    (list: List[Int])=> {
      val sortedUniqueList = list.distinct.sorted

      val heap = listToHeap(sortedUniqueList)
      val newlist = heapToList(heap)

      newlist == sortedUniqueList
    }
  }
}
