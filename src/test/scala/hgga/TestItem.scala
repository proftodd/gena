package hgga

import org.scalatest.FunSuite

/**
  * Created by john on 8/11/16.
  */
class TestItem extends FunSuite{

  test("Testing Item equality") {
    val item1 = new Item("a", 1.0)
    val item2 = new Item("b", 2.0)
    val item3 = new Item("a", 1.0)
    assert(item1 === item3)
    assert(item1 != item2)
  }

}
