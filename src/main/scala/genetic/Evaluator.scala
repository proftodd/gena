package genetic

/**
  * Created by john on 8/10/16.
  */
class Evaluator {
  var solutionBytes: Array[Byte] = _

  def load(solution: String) = {
    solutionBytes = new Array[Byte](solution.length)

    for (i <- 0 to solution.length - 1) {
      solutionBytes(i) = solution.charAt(i).asDigit.toByte
    }
  }

  def fittest(population: Population): Organism = {
    var o: Organism = population.pop(0)

    for (i <- 1 to population.size - 2) {
      val nextOrganism = population.pop(i)
      if (fitness(nextOrganism) > fitness(o)) {
        o = population.pop(i)
      }
    }

    o
  }

  def fitness(organism: Organism): Double = {
    var score: Integer = 0
    var index: Integer = 0

    for (gene <- organism.genes) {
      if (solutionBytes(index) == gene) score += 1
      index += 1
    }

    val maxScore = organism.genes.size
    1.0 - ((maxScore.toDouble - score.toDouble) / 100)
  }
}
