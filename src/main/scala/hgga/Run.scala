package hgga

import scala.collection.mutable.ArrayBuffer
import scala.util.Random

/**
  * Created by john on 8/16/16.
  */
object Run extends App {

  val r = new Random
  val itemNumber = 500
  val maxIter = 100
  var lastFitness = Double.MinValue
  val delta = 1e-6
  val itemList: List[Item] = for {
    i: Int <- (1 to itemNumber).toList
    n: String = r.alphanumeric.take(5).mkString
    v: Double = (r.nextDouble + 5) * 20
  } yield new Item(n, v)
  val pop = new Population(100, 0.66, 2)
  pop.initiate(150, itemList)
  val stats: ArrayBuffer[Tuple3[Int, Int, Double]] = ArrayBuffer()

  while (pop.fitness - lastFitness > delta && pop.iteration < maxIter) {
    lastFitness = pop.fitness
    pop.evolve
    stats += ((pop.iteration, pop.fittest.bins, pop.fitness))
  }

  println("Best packing:")
  println(pop.fittest)
  println("Fitness: " + pop.fitness)
  println("Iteration | Bins | Fitness")
  stats.foreach({ case (i, b, f) =>
    println(f"$i%9d | $b%4d | $f%1.2f")
  })
}
