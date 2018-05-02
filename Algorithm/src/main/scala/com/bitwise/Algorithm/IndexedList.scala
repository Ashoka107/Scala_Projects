package com.bitwise.Algorithm

import com.bitwise.Algorithm.IndexedList.NoSuchIndexExists

/**
  * Created by ashokk on 6/1/2016.
  */
trait IndexedList{
  def allKeysWithSameValueAs(index:Int):List[Int]
  def valueOf(index:Int):Int
  val keyValue:Map[Int,Int]
  def modify(is:List[Int],elem:Int):IndexedList
  def modify(index:Int,element:Int):IndexedList
}
object IndexedList {
  def apply(size:Int):IndexedList={
    if(size>0)
      NonEmptyIndexedList((1 to size).map(e=>(e,e)).toMap[Int,Int],size)
    else
      EmptyIndexedList()
  }
  def apply(map:Map[Int,Int]):IndexedList=NonEmptyIndexedList(map,map.size)
  case class NoSuchIndexExists extends RuntimeException
}
case class NonEmptyIndexedList(keyValue: Map[Int, Int], size: Int) extends IndexedList{
  override def valueOf(index:Int):Int=keyValue.get(index).getOrElse(throw new NoSuchIndexExists)
  override def allKeysWithSameValueAs(index:Int):List[Int]=keyValue.filter(e => e._2 == valueOf(index)).map(e=>e._1).toList
  override def modify(index:Int,elem:Int):IndexedList ={
    if(isValidIndex(index)&& isValidElement(elem))
      IndexedList(keyValue.updated(index,elem))
    else
      throw new NoSuchIndexExists
  }
  override def modify(list:List[Int],elem:Int):IndexedList= list match {
    case Nil => this
    case x::Nil => modify(x,elem)
    case x::tail => modify(x,elem).modify(tail,elem)
  }
  def isValidIndex(index:Int)=if(index>0 && index<=size)true else false
  def isValidElement(elem:Int)=isValidIndex(elem)
}
case class EmptyIndexedList() extends IndexedList{
override def allKeysWithSameValueAs(index:Int):List[Int]=List()
  override def valueOf(index:Int)=throw new IndexedList.NoSuchIndexExists
  override val keyValue:Map[Int,Int]=Map()
  override def modify(li:List[Int],elem:Int):IndexedList=modify(0,0)
  override def modify(index:Int,elem:Int):IndexedList=throw new IndexedList.NoSuchIndexExists
}
