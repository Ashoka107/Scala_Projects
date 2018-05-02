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
}
