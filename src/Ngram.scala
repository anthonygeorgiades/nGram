package ngramhomework

import scala.util.Random
import scala.io.Source
import scala.collection.mutable.HashMap
import scala.collection.mutable.ListBuffer

object NGram extends App {
  val stream = Source.fromFile(args(0)) //read in file with args 0 per Professor's request in Run → Run Configurations..., select your application, open the Arguments tab, and put the name of your book file in the Program arguments
  val all = stream.getLines.toList //takes stream and puts as a list of strings
  val allString = all.mkString //take list of string and make a string
  var arr = allString.toCharArray().toList //take the string and put into an array of char then put that into a list
  for (n <- 1 to 5) { //for loop with n grams for 1 through 5 grams
    val ngram = arr.sliding(n).toList //define an ngram by calling sliding iterator on arr and putting that into a list
    val mapx = new HashMap[List[Char], List[List[Char]]] //declare a hashmap, with key as a list of char and values as a list of list of chars
    for (i <- 1 to ngram.length - n - 1) { //a nested for loop, we basically will iterate up to the length of the list less the gram we are on less 1 (easy to show when drawn out, but we want to stop on a clean n-gram without extra characters)
      val currentGram = ngram(i) //declare our currentGram as the ngram at the ith location
      val nextGram = ngram(i + n) //our nextgram that will come subsequently after our ngram is not ngram+1 but ngram + n, since we move that many indexes to get to the next location (example in bossanova we want bos and san)
      if (!(mapx contains currentGram)) { //if our map doesn't contain the current gram
        mapx(currentGram) = List(nextGram) //we then place it in our map
      } else {
        mapx(currentGram) = mapx(currentGram) ++ List(nextGram) //if it does contain the current gram, we add the nextgram to the map
      }
    }
    val list = new ListBuffer[String]() //create a list that we will append to 
    var randomKey = mapx.keySet.toList(Random.nextInt(mapx.keySet.size)) //we need to start with a random key
    list += randomKey.mkString //add the randomkey to the list
    for (i <- 1 to (500 / n - 1)) { //we only want to print approxmately 500 characters, so we approximate with 500/n -1
      if (!(mapx.keySet contains randomKey)) { //if the map doesn't contain the randomkey, then of course we need a new random key
        var newrandomKey = mapx.keySet.toList(Random.nextInt(mapx.keySet.size)) //generate a random key
        randomKey = newrandomKey //set randomkey to the newrandomkey and loop over this again
      } else {
        val valueList = mapx(randomKey) //and once it does contain the randomkey, we find the list of values for that key
        val length = valueList.length //we get the length of the list of values for that key
        val randomValue = Random.nextInt(length) //we take a random value from that list of values
        val values = valueList(randomValue) //and we set that as our new value
        list += values.mkString //we add the value to our string list
        randomKey = values //and then set our values to our randomKey to revurse over againnnnn
      }
    }
    println(list mkString) //print out the entire list for n 1 through 5 grams per the request 
  }
}