package ASSIGNMENT
import scala.io.Source
import java.io.File
import scala.util.control.Breaks._

import scala.collection.immutable.Set
import scala.collection.mutable.Map
import scala.collection.immutable.List


object KFREQUENT {
     def main(args:Array[String]){
       
       
  var trans_entries : List[Set[String]] = List()        // no of transcations(list of set)
  var items_entries : Set[String] = Set()              // no of elements in dataSet(dealing with set instead of  list)
for (line_trans<-Source.fromFile("demo3.txt").getLines()) //  transactions are getting from file 

  {     
    val set_element = line_trans.trim.split(',').toSet 
      println("Transaction"+"  "+ set_element)// transcations entries splitted by commas and convert into set
    if (set_element.size > 0) 
    {
      trans_entries = trans_entries :+ set_element
      items_entries = items_entries ++ set_element
    }
  }

  var Return_item : Map[Set[String],Double] = Map()         // for return support as double
  var association_rule : List[(Set[String], Set[String], Double)] = List()

  //1
  
  def togetSupp(item_comb : Set[String]) : Double = {
    val counter = trans_entries.filter(transaction => item_comb.subsetOf(transaction)).size   //(counting support of each items in transactions) 
                                                                                                 //list size shows no of transactions in the end
    counter.toDouble / trans_entries.size.toDouble               //return support as double
  }

  
  //2
def Apriori(minimumSupport : Double = 0.5, minimumConfidence : Double = 0.6) = {
  var combination : Set[(Set[String], Double)] = Set()      //storing combination of items in the form of set
  var current_Set : Set[Set[String]] = items_entries.map( x => Set(x))
  var pair : Int = 2                        // how many items will be paired ,initially it takes 2 items as combination
  breakable {
    while (true) {

      val current_Combs : Set[(Set[String], Double)] = current_Set.map( x => (x, togetSupp(x)))
                                        .filter(y => (y._2 >= minimumSupport))           //support count of each item and check minimum support
                                        
      val current_L_Set = current_Combs.map(y => y._1).toSet                 
     
      if (current_L_Set.isEmpty) break                                      //check current_L_set is empty        
     
      current_Set = current_L_Set.map( x => current_L_Set.map(y => x | y)).reduceRight( (x, y) => x | y)
                                                                                                          .filter( x => (x.size==pair))
                                                                                                             //reducing in such a way there is no
                                                                                                           // dupication of pair
     combination = combination | current_Combs
      pair += 1
    }
  }
  for (item_no <- combination) {
    Return_item += (item_no._1 -> item_no._2)
  }
  Rule_calculate(minimumConfidence)   //calling of function 3 here
}


//3

  def Rule_calculate(minConfidence : Double = 0.6) = {
    Return_item.keys.foreach(item =>item.subsets.filter( x => (x.size<item.size & x.size>0))   //getting combinations using key function
          .foreach( subset => {association_rule = association_rule :+ (subset, item diff subset,
                                                                       Return_item(item).toDouble/Return_item(subset).toDouble)
                              }
                  )
    )
   association_rule = association_rule.filter( rule => rule._3>=minConfidence)   //filteirng assocaition rule acc to to the min confidence
  }
  

  
  Apriori(0.5, 0.6)
  println("items set with their support L1 and L2 acc to our dataset example")
  Return_item.foreach(println)
  println("Association Rules")
 association_rule.foreach(println)

}
}