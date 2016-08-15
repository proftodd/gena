package hgga

import scala.util.control.Breaks.{break, breakable}

/**
  * Created by john on 8/11/16.
  */
case class Bin(val capacity: Double, val itemList: List[Item]) {

  def filled: Double = itemList.map(i => i.value).sum

  def remaining: Double = capacity - filled

  def replace(i: Item): Option[List[Item]] = {
    var comboToReplace: Option[List[Item]] = None
    breakable {
      for (c <- 3 to 2 by -1) {
        val l: Option[List[Item]] = itemList.combinations(c).find(l1 => {
          val sum = l1.map(i1 => i1.value).sum
          (i.value > sum || i.value >= sum && l1.size > 1) && remaining + sum - i.value >= 0
        })
        if (!l.isEmpty) {
          comboToReplace = l
          break
        }
      }
    }
    comboToReplace
  }

  override def toString = itemList.mkString("[", ",", "]")
}

object Bin {

  def apply(capacity: Double, item: Item): Bin = Bin(capacity, List(item))
}
