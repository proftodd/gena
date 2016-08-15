package hgga

/**
  * Created by john on 8/11/16.
  */
case class Item(val name: String, val value: Double) {

  override def toString: String = "(" + name + "," + value + ")"

  override def equals(other: Any): Boolean = other match {
    case that: Item => {
      (that canEqual this) &&
      name == that.name &&
      value == that.value
    }
    case _ => false
  }

  def canEqual(other: Any): Boolean = other.isInstanceOf[Item]

  override def hashCode: Int = new Tuple2[String, Double](name, value).hashCode
}
