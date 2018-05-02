package com.bitwise.HuffmanTest

/**
  * Created by ashokk on 6/17/2016.
  */

import com.bitwise.HuffmanCoding.HuffmanCoding.{Leaf, Fork}
import org.scalatest.FunSuite
import com.bitwise.HuffmanCoding._
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner


@RunWith(classOf[JUnitRunner])
class HuffmanTest extends FunSuite {

  trait TestTrees {
    val t1 = Fork(Leaf('a', 2), Leaf('b', 3), List('a', 'b'), 5)
    val t2 = Fork(Fork(Leaf('a', 2), Leaf('b', 3), List('a', 'b'), 5), Leaf('d', 4), List('a', 'b', 'd'), 9)
  }


  test("weight of a larger tree") {
    new TestTrees {
      assert(HuffmanCoding.weight(t1) === 5)
    }
  }
  test("chars of a larger tree") {
    new TestTrees {
      assert(HuffmanCoding.chars(t2) === List('a','b','d'))
    }
  }
  test("string2chars"){
    new TestTrees{
      assert(HuffmanCoding.string2Chars("Ashok")===List('A','s','h','o','k'))
    }
  }
  test ("count of occurrence of chars in List"){
  new TestTrees {
    assert(HuffmanCoding.times(List('a','b','a','c','c','c','a','a'))===List(('b',1),('a',4),('c',3)))
  }

}
}