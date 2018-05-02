package com.bitwise.Algorithm

/**
  * Created by ashokk on 6/1/2016.
  */
trait QuickFind{
  val indexedList:IndexedList
  def union(start:Int,end:Int):QuickFind
  def connected(start:Int,end:Int):Boolean
}
object QuickFind {
def apply(size:Int)=NotConnectedQuickFind(size)
  def apply(indexedList: IndexedList)=ConnectedQuickFind(indexedList)
}

case class NotConnectedQuickFind(size: Int) extends QuickFind{
override def connected(start:Int,end:Int)=false
  override val indexedList=IndexedList(size)
  override def union(start:Int,end:Int):QuickFind={
    QuickFind(indexedList.modify(start,end))
  }
}

case class ConnectedQuickFind(indexedList: IndexedList) extends QuickFind{
override def union(start:Int,end:Int):QuickFind={
  QuickFind(indexedList.modify(indexedList.allKeysWithSameValueAs(start),end))
}
  override def connected(start:Int,end:Int):Boolean=indexedList.valueOf(start)==indexedList.valueOf(end)
}
