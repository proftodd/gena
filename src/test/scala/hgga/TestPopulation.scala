package hgga

import org.scalatest.FunSuite

/**
  * Created by john on 8/15/16.
  */
class TestPopulation extends FunSuite {

  def itemList: List[Item] = List(Item("1", 1), Item("2", 2), Item("3", 2), Item("4", 3), Item("5", 1),
                                  Item("6", 2), Item("7", 1), Item("8", 1), Item("9", 2), Item("10", 5))

  test("Initiate") {
    val pop = new Population(100, 0.66, 2)
    pop.initiate(7.0, itemList)
    pop.evolve
    assert(pop.population.forall(c => c.items.toSet === itemList.toSet))
  }
}
