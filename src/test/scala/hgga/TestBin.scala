package hgga

import org.scalatest.FunSuite

/**
  * Created by john on 8/15/16.
  */
class TestBin extends FunSuite {

  test("kataklinger's replace examples") {
    val bin1 = Bin(7.0, List(Item("6", 2), Item("5", 1), Item("3", 2)))
    val item1 = Item("10", 5)
    val combo1 = bin1.replace(item1)
    assert(!combo1.isEmpty)
    assert(combo1.get.toSet === Set(Item("6", 2), Item("5", 1), Item("3", 2)))

    val bin2 = Bin(7.0, List(Item("10", 5), Item("3", 2)))
    val item2 = Item("4", 3)
    assert(bin2.replace(item2).isEmpty)
  }
}
