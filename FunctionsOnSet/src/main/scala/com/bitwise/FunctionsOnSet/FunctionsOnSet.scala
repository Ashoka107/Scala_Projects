package com.bitwise.FunctionsOnSet

/**
  * Created by ashokk on 6/2/2016.
  */

class FunctionsOnSet  {
  type Set=Int=>Boolean
  def singletonSet(num:Int):Set={num1:Int=>num==num1}
  def contains(set:Set,num:Int):Boolean={set(num)}
  def union(s:Set,t:Set):Set={num:Int=>contains(s,num)||contains(t,num)}
  def intersect(s:Set,t:Set):Set={num:Int=>contains(s,num)&& contains(t,num)}
  def difference(s:Set,t:Set):Set={num:Int=>contains(s,num)&& !contains(t,num)}
  def filter(s:Set,p:Int=>Boolean):Set={num:Int=>contains(s,num)&&contains(p,num)}

  val bound=1000
 def forAll(s:Set,p:Int=>Boolean):Boolean ={
   def iter(a:Int):Boolean={
     if(a==bound)true
     else if(s(a)&& !p(a))false
     else iter(a+1)
   }
   iter(-bound)
 }
  def exist(s:Set,p:Int=>Boolean):Boolean={
    def iter(a:Int): Boolean ={
      if(a==bound)false
      else if(s(a) && p(a))true
      else iter(a+1)
    }
    iter(-bound)
  }
  def map(s:Set,f:Int=>Int):Set=i=>exist(s,j=>i==f(j))

  def toString(s:Set):String={
    val xs=for(i <- -bound to bound if(contains(s,i))) yield i
    xs.mkString("{", "," , "}")
  }
}
