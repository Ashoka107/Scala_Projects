package com.bitwise.HuffmanCoding

/**
  * Created by ashokk on 6/16/2016.
  */
object HuffmanCoding {

  trait CodeTree

  case class Fork(left: CodeTree, right: CodeTree, chars: List[Char], weight: Int) extends CodeTree

  case class Leaf(char: Char, weight: Int) extends CodeTree

  def weight(tree: CodeTree): Int = tree match {
    case Leaf(char, weight) => weight
    case Fork(left, right, chars, wt) => wt //+weight(left)+ weight(right)
    case _ => throw new NoSuchElementException
  }

  def chars(tree: CodeTree): List[Char] = tree match {
    case Leaf(char, weight) => List(char)
    case Fork(left, right, chs, wt) => chs //:::chars(left):::chars(right)
  }

  def makeCodeTree(left: CodeTree, right: CodeTree) = {
    Fork(left, right, chars(left) ::: chars(right), weight(left) + weight(right))
  }

  def string2Chars(str: String): List[Char] = str.toList

  def times(list:List[Char] ): List[(Char, Int)] ={
   list.groupBy(identity).mapValues(_.size).toList
  }
}
class createCodeTree {

}
