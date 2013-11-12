	package quickcheck

import common._

import org.scalacheck._
import Arbitrary._
import Gen._
import Prop._

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {

  property("min1") = forAll { a: Int =>
    val h = insert(a, empty)
    findMin(h) == a
  }
  
  property("min2") = forAll { (a: Int, b :Int) =>
    val h = insert(b, insert(a, empty))
    a > b match{
      case true => findMin(h) == b
      case false => findMin(h) == a
    }
  }
  
  property("min3") = forAll { a: Int =>
    val h = deleteMin(insert(a, empty))
    isEmpty(h)
  }
  
  property("min4") = forAll { (a: Int, b :Int) =>
    val h = meld(insert(a, empty), insert(b, empty))
    a > b match{
      case true => findMin(h) == b
      case false => findMin(h) == a
    }
  }
  
  property("min5") = forAll { (a: Int, b :Int) =>
    val h = deleteMin(insert(a, insert(b, empty)))
    a > b match{
      case true => findMin(h) == a
      case false => findMin(h) == b
    }
  }
  
  property("min6") = forAll { h: H =>
    def iterate(h : H, a : Int) : Boolean = isEmpty(h) match{
      case true => true
      case false if(a > findMin(h) ) => false
      case false => iterate(deleteMin(h), findMin(h))
    }
    iterate(deleteMin(h), findMin(h))
  }
  
  property("min7") = forAll { h: H =>
    val t = findMin(h)
    if (isEmpty( deleteMin(h) )) 
      true 
    else
    {
      val u = findMin(deleteMin(h))
      t <= u
    }
  }
  
  property("min8") = forAll { (a: Int, b: Int, c: Int) =>
      val h = insert(a, insert(b, insert(c, empty)))
      val s = List(a, b, c).sorted
      findMin(deleteMin(h)) == s(1)
  }
  
  lazy val genHeap: Gen[H] = for {
    a <- arbitrary[Int]
    h <- oneOf(value(empty), genHeap)
    } yield insert(a, h)	
    
  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

}
