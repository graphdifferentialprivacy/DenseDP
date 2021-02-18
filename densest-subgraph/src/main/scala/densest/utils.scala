package densest

import scala.collection.mutable.Map
import scala.collection.mutable.Set
import scala.collection.mutable.ArrayBuffer
import scala.util.Random
import scala.math
import scala.io.Source
import scala.collection.parallel.ForkJoinTaskSupport
import scala.collection.parallel.mutable.ParMap
import scala.collection.mutable.HashMap
import scala.collection.parallel.ThreadPoolTaskSupport

object utils {

  def main(args: Array[String]): Unit = {
    val n = 1000000
    
    val graph : Map[Int, Set[Int]] = Map()

    for (i <- 1 to 20) {
      for (i <- 1 to n) {
        graph.put(Random.nextInt, Set(Random.nextInt(10)))
      }
      val start = System.currentTimeMillis()
      rho(graph)
      val end = System.currentTimeMillis()
      println(s"Seq Iter $i: " + (end - start))
      val start2 = System.currentTimeMillis()
      rhoPar(graph.par)
      val end2 = System.currentTimeMillis()
      println(s"Par Iter $i: " + (end2 - start2))
    }
  }

  def rho(subGraph: Map[Int, Set[Int]]): Double = {
    val nNodes = subGraph size
    val degSum = subGraph.map(p => (p._2.size)).sum
    return (degSum.toDouble / 2) / nNodes
  }

  def rhoPar(subGraph: ParMap[Int, Set[Int]]): Double = {
    val nNodes = subGraph.size
    //subGraph.tasksupport = new ForkJoinTaskSupport(new java.util.concurrent.ForkJoinPool(8))
    val degSum = subGraph.map(p => (p._2.size)).sum
    return (degSum.toDouble / 2) / nNodes
  }

  def loadGraph(filename: String): Map[Int, Set[Int]] = {
    val file = Source.fromFile(filename)
    val lines = file.getLines.toArray
    file.close

    val graph: Map[Int, Set[Int]] = Map()

    lines.foreach(parseLine(_, graph))

    //println(lines.size)

    return graph
  }

  def parseLine(line: String, graph: Map[Int, Set[Int]]): Unit = {
    if (line.startsWith("#"))
      return

    val edge = line.split("[\\s,]+").map(_.toInt)

    for (i <- 0 to 1) {
      graph.getOrElseUpdate(edge(i), Set()).add(edge(1 - i))
    }

    return
  }

  final def sampleRho(dist: ArrayBuffer[Double], epsilon: Double): (Double, Int) = {
    val p = Random.nextDouble * dist.map(p => math.exp(epsilon * p)).sum
    //println(p, dist.map(p => math.exp(epsilon * p)).sum)
    val iter = dist.iterator
    var accumulative = 0.0
    var index = 0
    while (iter.hasNext) {
      index += 1
      val curr = iter.next
      accumulative += math.exp(epsilon * curr)
      //println("Accumulative:" + accumulative + " curr: " + math.exp(epsilon * curr))
      if (accumulative >= p)
        return (curr, index)
    }
    sys.error(f"Distribution error") // needed so it will compile
  }

  def cloneGraph(graph: Map[Int, Set[Int]]): HashMap[Int, Set[Int]] = {
    val clone: HashMap[Int, Set[Int]] = HashMap()
    graph.foreach(p => clone.put(p._1, p._2.clone()))
    return clone
  }

}
