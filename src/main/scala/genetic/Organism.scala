package genetic

/**
  * Created by john on 8/10/16.
  */
class Organism(chromosome: Array[Byte]) {

  def genes = chromosome

  override def toString: String = {
    val sb: StringBuilder = new StringBuilder
    for (gene <- chromosome) {
      if (gene == 1) sb.append("1") else sb.append("0")
    }
    sb.toString
  }
}
