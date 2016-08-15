package hgga

import org.scalacheck.{Arbitrary, Gen, Properties}
import org.scalacheck.Prop.{forAll, BooleanOperators}

import scala.util.Random

/**
  * Created by john on 8/15/16.
  */
object ChromosomeSpecification extends Properties("Chromosome") {

  def r = new Random()

  implicit def arbItem: Arbitrary[Item] = Arbitrary {
    for {
      name <- Arbitrary.arbitrary[String]
      value = r.nextDouble() * 10 + 1
    } yield Item(name, value)
  }

  implicit def arbItemList: Arbitrary[List[Item]] = Arbitrary{ Gen.containerOf[List, Item](arbItem.arbitrary) }

  implicit def arbChrom: Arbitrary[Chromosome] = Arbitrary {
    for {
      itemList <- arbItemList.arbitrary
      capacity = (r.nextDouble() * 20 + 1) * 50 + 1
    } yield Chromosome(capacity, itemList)
  }

  property("mutation does not change contents") = forAll { c: Chromosome =>
    val c1: Chromosome = c.mutate(2)
    c.items.toSet == c1.items.toSet
  }

  property("crossover does not change contents") = forAll { c: Chromosome =>
    val c2 = Chromosome(c.binCapacity, r.shuffle(c.items))
    val childList = c crossover c2
    childList.size == 2
    c.items.toSet == c2.items.toSet
    c.items.toSet == childList.head.items.toSet
    c.items.toSet == childList.tail.head.items.toSet
  }
}
