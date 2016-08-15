package hgga

import scala.collection.mutable.ArrayBuffer
import scala.util.Random

/**
  * Created by john on 8/15/16.
  */
class Population(val populationSize: Int, mutationProb: Double, mutationFreq: Int) {

  val r = new Random
  var iteration: Int = 0
  var fitness = 0.0
  var fittest: Chromosome = _
  var population: ArrayBuffer[Chromosome] = new ArrayBuffer(populationSize)

  def initiate(binCapacity: Double, itemList: List[Item]) = {

    for (i <- 1 to populationSize) {
      population += Chromosome(binCapacity, Random.shuffle(itemList))
    }
    iteration = 0
  }

  def evolve() = {
    val (parent1, parent2) = parentSelect()
    population.clear
    (1 to populationSize / 2).foreach(i => {
      val children = parent1 crossover parent2
      children.foreach(c => {
        if (r.nextDouble < mutationProb) {
          population += c.mutate(mutationFreq)
        } else {
          population += c
        }
      })
    })
    iteration = iteration + 1
    fittest = population.maxBy(c => c.fitness)
    fitness = fittest.fitness
  }

  def parentSelect(): (Chromosome, Chromosome) = {
    val selection = rouletteNormalized(population.toList).sortBy({ case (p, c) => p })
    val parent1Pool = parentPool(selection).sortWith((c1, c2) => c2.fitness > c1.fitness)
    val parent2Pool = parentPool(selection).sortWith((c1, c2) => c2.fitness > c1.fitness)
    (parent1Pool.head, parent2Pool.head)
  }

  def rouletteNormalized(population: List[Chromosome]): List[(Double, Chromosome)] = {
    val wheel = rouletteCDF(population)
    val sum = wheel.map({ case (p, c) => p}).sum
    wheel.map({ case (p, c) => (p / sum, c) }).sortBy({ case (p, c) => p})
  }

  def rouletteCDF(population: List[Chromosome]): List[(Double, Chromosome)] = population match {
    case Nil => Nil
    case (p :: Nil) => (p.fitness, p) :: Nil
    case (p :: ps) => {
      val prev = rouletteCDF(ps)
      (p.fitness + prev.head._2.fitness, p) :: prev
    }
  }

  def parentPool(selection: List[(Double, Chromosome)]): List[Chromosome] = {
    (1 to 3).map(i => {
      val dice = r.nextDouble
      selection.find({ case (p, c) => dice <= p} )
    }).map(o => o.getOrElse(selection.last)).map({ case (p, c) => c }).toList
  }
}

object Population {
  
}
