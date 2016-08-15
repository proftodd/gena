package hgga

import scala.util.Random
import scala.util.control.Breaks.{break, breakable}

/**
  * Created by john on 8/11/16.
  *
  * Based on the description of the Bin Packing Problem by Mladen Jankovic
  *
  * http://kataklinger.com/index.php/genetic-algorithm-bin-packing/
  */
class Chromosome (val binCapacity: Double, val binList: List[Bin]) {

  def FITNESS_POWER: Double = 2.0

  def addItem(item: Item): Chromosome = {
    binList.find(b => b.remaining >= item.value) match {
      case Some(b) => {
        val newBin = new Bin(b.capacity, item :: b.itemList)
        new Chromosome(binCapacity, newBin :: binList diff List(b))
      }
      case None => {
        val newBin: Bin = new Bin(binCapacity, List(item))
        new Chromosome(binCapacity, newBin :: binList)
      }
    }
  }

  def fitness: Double = {
    if (binList.isEmpty) 0.0
    else {
      val filledList = binList.map(b => b.itemList.map(i => i.value).sum / b.capacity)
      val numerator = filledList.map(v => Math.pow(v, FITNESS_POWER)).sum
      numerator / binList.size
    }
  }

  def itemsInBins(l: List[Bin]): List[Item] = l.flatMap(b => b.itemList)

  def binsWithItems(l: List[Item]): List[Bin] = {
    binList.filter(b => {
      val intersection = l.intersect(b.itemList)
      !(intersection.isEmpty)
    })
  }

  def crossoverPoints(): (Int, Int) = {
    val first = Random.nextInt(binList.size)
    val second = Random.nextInt(binList.size)
    if (first <= second) (first, second)
    else (second, first)
  }

  def splice(start: Int, end: Int, insert: List[Bin]): Chromosome = {
    val newItems: List[Item] = insert.flatMap(b => b.itemList)
    val binsWithDups = binsWithItems(newItems)
    val unassignedItems = binsWithDups.flatMap(b => b.itemList) diff newItems
    val leftBinList = binList.take(start) diff binsWithDups
    val rightBinList = binList.takeRight(binList.length - end) diff binsWithDups
    var c = new Chromosome(binCapacity, leftBinList ++ insert ++ rightBinList)
    var displacedItems: List[Item] = List()
    for (i <- unassignedItems) {
      val (newCh, moreDisplacedItems) = c.replace(i)
      c = newCh
      displacedItems ++= moreDisplacedItems
    }
    displacedItems.sortBy(i => i.value).foreach(i => c = c.addItem(i))
    c
  }

  def replace(i: Item): (Chromosome, List[Item]) = {
    var comboToReplace: Option[(Bin, List[Item])] = None
    breakable {
      for (b <- binList) {
        val l = b.replace(i)
        if (!l.isEmpty) {
          comboToReplace = new Some(b, l.get)
          break
        }
      }
    }
    comboToReplace match {
      case Some((b, l)) => {
        val newBin = Bin(b.capacity, i :: b.itemList diff l)
        (new Chromosome(this.binCapacity, newBin :: (binList diff List(b))), l)
      }
      case None => (this, List(i))
    }
  }

  def crossover(that: Chromosome): List[Chromosome] = {
    val myCrossoverPoints = crossoverPoints()
    val itsCrossoverPoints = that.crossoverPoints()
    val mySlice = binList.slice(myCrossoverPoints._1, myCrossoverPoints._2)
    val itsSlice = that.binList.slice(itsCrossoverPoints._1, itsCrossoverPoints._2)
    val child1 = splice(myCrossoverPoints._1, myCrossoverPoints._2, itsSlice)
    val child2 = that.splice(itsCrossoverPoints._1, itsCrossoverPoints._2, mySlice)
    List(child1, child2)
  }

  def mutate(l: List[Int]): Chromosome = {
    val deletedBins: List[Bin] = for {i <- l} yield binList.slice(i, i + 1).head
    val keptBins = binList diff deletedBins
    var c: Chromosome = new Chromosome(binCapacity, keptBins)
    var displacedItems: List[Item] = List()
    for (i <- itemsInBins(deletedBins)) {
      val (newCh, moreDisplacedItems) = c.replace(i)
      c = newCh
      displacedItems ++= moreDisplacedItems
    }
    displacedItems = displacedItems.sortBy(i => i.value)
    displacedItems.foreach(i => c = c.addItem(i))
    c
  }

  override def toString = "Chromosome: [" + binCapacity + ", " + binList.mkString("{", ";" , "}") + "]"
}

object Chromosome {

  def apply(binCapacity: Double, itemList: List[Item]): Chromosome = itemList match {
    case Nil => new Chromosome(binCapacity, List())
    case i :: Nil => new Chromosome(binCapacity, List(new Bin(binCapacity, List(i))))
    case i :: is => apply(binCapacity, is).addItem(i)
  }
}
