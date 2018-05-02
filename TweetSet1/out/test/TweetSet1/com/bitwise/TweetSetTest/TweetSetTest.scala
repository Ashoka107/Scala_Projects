package com.bitwise.TweetSetTest

/**
  * Created by ashokk on 6/7/2016.
  */
package objsets

import com.bitwise.TweetSet.{TweetSet, Tweet, Empty}
import org.junit.{Assert, Test}
import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class TweetSetTest extends FunSuite {
  trait TestSets {
    val set1 = new Empty
    val set2 = set1.incl(new Tweet("a", "a body", 20))
    val set3 = set2.incl(new Tweet("b", "b body", 40))
    val c = new Tweet("c", "c body", 7)
    val d = new Tweet("d", "d body", 9)
    val set4c = set3.incl(c)
    val set4d = set3.incl(d)
    val set5 = set4c.incl(d)
  }

  def asSet(tweets: TweetSet): Set[Tweet] = {
    var res = Set[Tweet]()
    tweets.foreach(res += _)
    res
  }

  def size(set: TweetSet): Int = asSet(set).size

  test("filter: on empty set") {
    new TestSets {
      assert(size(set1.filter(tw => tw.user =="a")) === 0)
    }
  }

  test("filter: On NonEmptySet Set5"){
    new TestSets {
    assert(size(set4c.filter(tw =>  tw.retweets>=20)) === 2)
    }
  }
  test("union"){
    new TestSets {
      assert(size(set4c.union(set4d))===4)
    }
  }
  test("remove"){
    new TestSets {
      assert(size(set4d.remove(d))==2)
    }
  }
}
