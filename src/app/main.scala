package app

import scala.annotation.tailrec
import scala.collection.mutable.ListBuffer
import scala.util.Random

object main extends App {

  case class Pairs(picker: String, picked: String)

  def readCsv(directory: String): List[String] = {
    var listOfNames = new ListBuffer[String]()
    val bufferedSource = io.Source.fromFile(s"$directory")
    for (line <- bufferedSource.getLines) {
      val name = line.split(",").map(_.trim)
      listOfNames += name(0)
    }
    bufferedSource.close
    listOfNames.toList
  }

  def randomNumber(size: Int): Int = new Random().nextInt(size)

  def pickName(pickers: String, toBePicked: String): Pairs = Pairs(pickers, toBePicked)

  def assignPairs(list: List[String], list1: List[String]): List[Pairs] = {
    var listOfPairs = new ListBuffer[Pairs]()
    var listToBePicked = new ListBuffer[String]()
    list1.foreach(x => listToBePicked += x)
    for (person <- list) {
      pickMethod()
      @tailrec
      def pickMethod(): Unit = {
        val temp = listToBePicked(randomNumber(listToBePicked.size))
        if (!areSamePerson(person, temp)) {
          listOfPairs += pickName(person, temp)
          if (listToBePicked.toList.nonEmpty) listToBePicked -= temp
        } else pickMethod()
      }
    }
    listOfPairs.toList
  }

  def areSamePerson(person1: String, person2: String): Boolean = if (person1 == person2) true else false

  def printList(message: String, list: List[String]): Unit = {
    println(message)
    list.foreach(x => println(x))
  }

  val listOfNames: List[String] = readCsv("src/CSVtest.csv")
  printList("List of pickers:", listOfNames)
  val namesToBePicked: List[String] = listOfNames
  printList("List of people to be picked: ", namesToBePicked)
  println(assignPairs(listOfNames, namesToBePicked))


  //todo print to terminal or create csv file


}