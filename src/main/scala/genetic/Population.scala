package genetic

import scala.util.Random

/**
  * Created by john on 8/10/16.
  */
class Population(populationSize: Integer) {

  val chromosomeSize: Integer = 64
  var pop: Array[Organism] = _

  val mutationRate = 0.015
  val mixingRatio = 0.5

  def initialize = pop = new Array[Organism](populationSize + 1)

  def populate = {
    initialize
    for (i <- 0 to populationSize - 1) {
      val bytes = new Array[Byte](chromosomeSize)
      for (j <- 0 to chromosomeSize - 1) {
        bytes(j) = Math.round(Math.random).toByte
      }
      val organism = new Organism(bytes)
      pop(i) = organism
    }
  }

  def size: Integer = pop.length

  def addOrganism(index: Integer, organism: Organism) =
    pop(index) = organism

  def evolve(elitist: Boolean, evaluator: Evaluator): Population = {
    val nextGeneration = new Population(pop.size)
    nextGeneration.initialize

    var offset = 0

    if (elitist) {
      val eliteOrganism = evaluator.fittest(this)
      nextGeneration.addOrganism(0, mutate(eliteOrganism))
      offset += 1
    }

    for (index <- offset to pop.size) {
      val parent1: Organism = select(evaluator)
      val parent2: Organism = select(evaluator)
      val child: Organism = crossover(parent1, parent2)
      nextGeneration.addOrganism(index, mutate(child))
    }

    nextGeneration
  }

  def mutate(organism: Organism): Organism = {
    val c: Array[Byte] = organism.genes
    for (index <- 0 to c.length - 1) {
      if (Math.random <= mutationRate) {
        c(index) = Math.round(Math.random).toByte
      }
    }
    new Organism(c)
  }

  def crossover(parent1: Organism, parent2: Organism): Organism = {
    val otherParentGenes = parent2.genes
    val chromosomes = new Array[Byte](otherParentGenes.length)

    var index: Integer = 0
    for (gene <- parent1.genes) {
      if (Math.random <= mixingRatio) {
        chromosomes(index) = gene
      } else {
        chromosomes(index) = otherParentGenes(index)
      }
      index += 1
    }

    new Organism(chromosomes)
  }

  def select(evaluator: Evaluator): Organism = {
    val numberOfRounds = 10

    val tournament = new Population(numberOfRounds)
    tournament.initialize

    for (i <- 0 to numberOfRounds) {
      val randomOrganism = pop(Random.nextInt(populationSize))
      tournament.addOrganism(i, randomOrganism)
    }

    evaluator.fittest(tournament)
  }

  override def toString: String = {
    val sb: StringBuilder = new StringBuilder

    sb.append("[")
    for (organism <- pop) {
      sb.append(organism + ", ")
    }
    sb.dropRight(2)
    sb.append("]")
    sb.toString
  }
}
