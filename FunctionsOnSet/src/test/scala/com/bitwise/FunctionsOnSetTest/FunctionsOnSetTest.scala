package com.bitwise.FunctionsOnSetTest

import com.bitwise.FunctionsOnSet.FunctionsOnSet
import org.junit.Test
import org.scalatest.Matchers._

/**
  * Created by ashokk on 6/2/2016.
  */
class FunctionsOnSetTest {
@Test
  def itShouldCreateSingletonSet(): Unit ={
  //given
  val fun=new FunctionsOnSet()
  val set=fun.singletonSet(2)
  //when
  val check=fun.contains(set,2)
  //then
  check shouldEqual  true
}
  @Test
  def itShouldExtractUnionElements(): Unit ={
    //given
    val fun=new FunctionsOnSet
    val set1=fun.singletonSet(2)
    val set2=fun.singletonSet(3)
    val set3=fun.singletonSet(2)
    //when
    val unionset=fun.union(set1,set3)
   // val cont=fun.contains(unionset,2)
    //val cont2=fun.contains(unionset,3)
    //then
    //cont shouldBe(true)
    //cont2 shouldBe(true)
    unionset shouldEqual Set(2)
  }
}
