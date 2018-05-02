package com.bitwise.TweetSet

import org.omg.PortableInterceptor.NON_EXISTENT

/**
  * Created by ashokk on 6/7/2016.
  */
trait TweetSet {

    def filter(p:Tweet => Boolean):TweetSet=filterAcc(p,new Empty)
    def filterAcc(p:Tweet=> Boolean,acc:TweetSet):TweetSet
    def union(that:TweetSet):TweetSet
    def incl(tweet: Tweet): TweetSet
     def contains(tweet:Tweet):Boolean
     def mostRetweeted:Tweet
     def remove(tweet:Tweet):TweetSet
     def foreach(P: Tweet => Unit):Unit
     def descendingByRetweets:TweetList
  def isEmpty:Boolean

    }
    class Tweet(val user:String,val text:String,val retweets:Int){
            override def toString: String =
            "User: "+ user + "\n"+
            "text: "+text+ " ["+retweets+"]"
            }
    class Empty extends TweetSet{
      def isEmpty:Boolean=true
       def filterAcc(p:Tweet=>Boolean,acc:TweetSet):TweetSet=acc
       def union(that:TweetSet):TweetSet=that
       def contains(tweet: Tweet):Boolean=false
      def incl(tweet:Tweet):TweetSet=new NonEmpty(tweet,new Empty,new Empty)
      override def remove(tweet: Tweet):TweetSet=this
      override def foreach(P: Tweet => Unit): Unit = ()
      override def mostRetweeted=throw new TweetNotFoundException
      override def descendingByRetweets= throw new TweetNotFoundException


    }
    class NonEmpty(elem:Tweet,left:TweetSet,right:TweetSet) extends TweetSet{
      def isEmpty:Boolean=false
      def union(that:TweetSet):TweetSet= {
       left.union(right.union(that incl elem))
      }
     def filterAcc(p: Tweet => Boolean, acc: TweetSet): TweetSet =
        {
         left.filterAcc(p,right filterAcc(p,if(p(elem))acc incl elem else acc))
        }
      def maxTweet(a: Tweet, b: Tweet): Tweet = {
        if (a.retweets > b.retweets) a else b
      }
      def mostRetweeted: Tweet = {
        if (left.isEmpty && right.isEmpty) elem
        else if (left.isEmpty) maxTweet(right.mostRetweeted, elem)
        else if (right.isEmpty) maxTweet(left.mostRetweeted, elem)
        else maxTweet(maxTweet(left.mostRetweeted, elem), right.mostRetweeted)
      }
      def descendingByRetweets: TweetList = {
        new Cons(elem, remove(elem).descendingByRetweets)
      }
       def contains(tweet:Tweet):Boolean={
        if(tweet.text < elem.text) left.contains(tweet)
        else if(elem.text < tweet.text) right.contains(tweet)
        else true
            }
     def incl(tweet: Tweet):TweetSet={
        if(tweet.text < elem.text) new NonEmpty(elem,left.incl(tweet),right)
        else if(elem.text < tweet.text) new NonEmpty(elem,left,right.incl(tweet))
        else this
      }
      override def remove(tweet: Tweet):TweetSet={
        if(tweet.text < elem.text)new NonEmpty(elem, left.remove(tweet),right)
        else if(elem.text < tweet.text) new NonEmpty(elem,left,right.remove(tweet))
        else left.union(right)
      }
      def foreach(f: Tweet => Unit): Unit = {
        f(elem)
        left.foreach(f)
        right.foreach(f)
      }
    }
trait TweetList {
  def head: Tweet
  def tail: TweetList
  def isEmpty: Boolean
  def foreach(f: Tweet => Unit): Unit =
    if (!isEmpty) {
      f(head)
      tail.foreach(f)
    }
}
object Nil extends TweetList {
  def head = throw new java.util.NoSuchElementException("head of EmptyList")
  def tail = throw new java.util.NoSuchElementException("tail of EmptyList")
  def isEmpty = true
}

class Cons(val head: Tweet, val tail: TweetList) extends TweetList {
  def isEmpty = false
}


object GoogleVsApple {
  val google = List("android", "Android", "galaxy", "Galaxy", "nexus", "Nexus")
  val apple = List("ios", "iOS", "iphone", "iPhone", "ipad", "iPad")

  lazy val googleTweets: TweetSet = ???
  lazy val appleTweets: TweetSet = ???

  /**
    * A list of all tweets mentioning a keyword from either apple or google,
    * sorted by the number of retweets.
    */
  lazy val trending: TweetList = ???
}

object Main extends App {
  // Print the trending tweets
  GoogleVsApple.trending foreach println

}
