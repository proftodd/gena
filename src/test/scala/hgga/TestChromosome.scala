package hgga

import org.scalatest.{BeforeAndAfter, FunSuite}

/**
  * Created by john on 8/11/16.
  */
class TestChromosome extends FunSuite with BeforeAndAfter {

  val binList: List[Bin] = List(Bin(5.0, List(Item("e", 5))),
                                Bin(5.0, List(Item("a", 1), Item("d", 4))),
                                Bin(5.0, List(Item("c", 3), Item("b", 2))))
  val binList1234: List[Bin] = List(Bin(7.0, List(Item("1", 1), Item("2", 2), Item("4", 3))),
                                    Bin(7.0, List(Item("3", 2), Item("5", 1), Item("6", 2))),
                                    Bin(7.0, List(Item("7", 1), Item("8", 1), Item("9", 2))),
                                    Bin(7.0, List(Item("10", 5))))
  val binListABCD: List[Bin] = List(Bin(7.0, List(Item("10", 5), Item("1", 1))),
                                    Bin(7.0, List(Item("6", 2), Item("5", 1), Item("9", 2))),
                                    Bin(7.0, List(Item("3", 2), Item("2", 2))),
                                    Bin(7.0, List(Item("8", 1), Item("7", 1), Item("4", 3))))
  val empty = Chromosome(5.0, List())
  val filled = new Chromosome(5.0, binList)
  val chrom1234: Chromosome = new Chromosome(7.0, binList1234)
  val chromABCD: Chromosome = new Chromosome(7.0, binListABCD)

  test("Empty chromosome has 0.0 fitness") {
    assert(empty.fitness === 0.0)
  }

  test("Chromosome with 5 items has 3 bins and optimal fitness") {
    assert(filled.binList.size === 3)
    assert(filled.fitness === 1.0)
  }

  test("Partially-filled Chromosome has fitness between 0.0 and 1.0") {
    val binList = List(Bin(5.0, List(Item("e", 5))),
                       Bin(5.0, List(Item("b1", 2), Item("b2", 2))),
                       Bin(5.0, List(Item("c", 3))))
    val ch = new Chromosome(5.0, binList)
    assert(ch.fitness === 2.0 / 3)
  }

  test("Chromosome.itemsInBins") {
    val binList: List[Bin] = List(filled.binList.tail.head)
    assert(filled.itemsInBins(binList).toSet === Set(Item("a", 1), Item("d", 4)))
  }

  test("Chromosome.binsWithItems") {
    val itemList: List[Item] = List(Item("a", 1), Item("e", 5))
    assert(filled.binsWithItems(itemList) === filled.binList.head :: filled.binList.tail.head :: Nil)
  }

  test("kataklinger's crossover example") {
    val slice1 = binList1234.slice(1, 3)
    val slice2 = binListABCD.slice(1, 2)
    val child1 = chrom1234.splice(slice2)
    val child2 = chromABCD.splice(slice1)
    assert(testChromosome(child1))
    assert(testChromosome(child2))
  }

  test("test crossover with a zero-length splice") {
    val splicePoint1 = (1, 1)
    val splicePoint2 = (1, 2)
    val slice1 = binList1234.slice(1, 1)
    val slice2 = binListABCD.slice(1, 2)
    val child1 = chrom1234.splice(slice2)
    val child2 = chromABCD.splice(slice1)
    assert(testChromosome(child1))
    assert(testChromosome(child2))
  }

  test("test crossover at index 0") {
    val splicePoint1 = (0, 1)
    val splicePoint2 = (0, 2)
    val slice1 = binList1234.slice(0, 1)
    val slice2 = binListABCD.slice(0, 2)
    val child1 = chrom1234.splice(slice2)
    val child2 = chromABCD.splice(slice1)
    assert(testChromosome(child1))
    assert(testChromosome(child2))
  }

  test("kataklinger's replace example") {
    val binList = List(Bin(7.0, List(Item("6", 2), Item("5", 1), Item("3", 2))),
                       Bin(7.0, List(Item("9", 2), Item("8", 1), Item("7", 1))))
    val ch = new Chromosome(7.0, binList)
    val item = Item("10", 5)
    val (newCh, displaced) = ch.replace(item)
    assert(displaced.toSet === Set(Item("3", 2), Item("5", 1), Item("6", 2)))
    val item2 = Item("4", 3)
    val (newCh2, displaced2) = newCh.replace(item2)
    val newDisplaced = displaced ++ displaced2
    assert(newDisplaced.toSet === Set(Item("3", 2), Item("5", 1), Item("6", 2), Item("9", 2), Item("8", 1)))
  }

  test("mutate") {
    val binList = List(Bin(7.0, List(Item("10", 5), Item("1", 1))),
                       Bin(7.0, List(Item("6", 2), Item("5", 1), Item("3", 2))),
                       Bin(7.0, List(Item("9", 2), Item("8", 1), Item("7", 1), Item("4", 3))),
                       Bin(7.0, List(Item("2", 2))))
    val ch = new Chromosome(7.0, binList)
    val newCh = ch.mutate(Set(0, 3))
    assert(testChromosome(newCh))

    val newCh2 = ch.mutate(Set(1, 1))
    assert(testChromosome(newCh2))

    val oneBinList = List(Bin(7.0, List(Item("10", 5), Item("1", 1))))
    val ch2 = new Chromosome(7.0, oneBinList)
    val newCh3 = ch2.mutate(3)
    assert(newCh3.items.toSet === oneBinList.flatMap(b => b.itemList).toSet)
  }

  private def testChromosome(ch: Chromosome): Boolean = {
    val expectedItemList = (1 to 10).map(i => Integer.toString(i))
    val itemList: List[String] = ch.items.map(i => i.name)
    (itemList.size == expectedItemList.size) && (itemList.toSet == expectedItemList.toSet)
  }
}
