
package densest

import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.Map
import scala.collection.mutable.Set
import scala.collection.mutable.ArrayBuffer
import scala.util.Random

object phase {
  
  def main(args: Array[String]): Unit = {
    val graph = utils.loadGraph("data/ca-GrQc.txt")
    
    val outputs : ArrayBuffer[Double] = ArrayBuffer()
    val iters: ArrayBuffer[Int] = ArrayBuffer()
    
    for (i <- 1 to 4) {
      val (density, subgraph, iter) = parPhase(utils.cloneGraph(graph), 16, math.pow(10, -6))
      outputs += density
      iters += iter
//      println(s"Subgraph: $subgraph")
    }
    
    val mean = outputs.sum / outputs.size
    println("Mean Density: " + mean)
    println("Std Density: " + math.sqrt(outputs.map(p => math.pow(p - mean, 2)).sum / (outputs.size-1)))

    val meanIter = iters.sum / iters.size
    println("Mean Iter: " + meanIter)
    println("Std Iter: " + math.sqrt(iters.map(p => math.pow(p - meanIter, 2)).sum / (iters.size-1)))
  }

  
  def sample(degree: Int, epsilon: Double): Boolean = {
    val prob = math.exp(-(epsilon * (degree + 1/epsilon + 1)))
    val p = Random.nextDouble
    if (p <= prob) {
      return true
    }
    return false
  }
  
  def quickSample(degrees: scala.collection.Map[Int, Int], epsilon: Double): scala.collection.Map[Int, Int] = {
    val sampledIters = degrees.map(p => (p._1, geometricSample(math.exp(-(epsilon * (p._2 + 1/epsilon + 1))))))

    return sampledIters
  }
  
  def geometricSample(p: Double): Int = {
    return math.ceil(math.log(Random.nextDouble()) / math.log(1 - p)).toInt
  }

  def Lap(labda: Double): Double = {
    return labda * math.log(Random.nextDouble() / Random.nextDouble())
  }
  
  def parPhase(graph: Map[Int, Set[Int]], epsilon: Double, delta: Double): (Double, scala.collection.Set[Int], Int) = {
    val nNodes = graph.size
    val densitySubGraphs: ArrayBuffer[Double] = ArrayBuffer()

    val nodes: Set[Int] = Set()
    graph.keySet.foreach(p => nodes.add(p))

    val epsilonP = epsilon * (1 - 1/math.E) / (24 * math.log(4/delta))
    println("epsilonP", epsilonP)

    val removedNodes: ArrayBuffer[scala.collection.Set[Int]] = ArrayBuffer() 

    var countIter = 0
    var lastRemoved = 0
    var phaseI = 0

    // while loop execution
    while (graph.size > 0) {
      countIter += 1
      val density = utils.rho(graph)

      if (graph.size <= math.log(nNodes)) {
        // Put all remaining nodes to 1 single graph
        println("final phase:", graph.size, nNodes, math.log(nNodes), "density:", density)
        densitySubGraphs += density
        removedNodes += graph.keySet
        graph --= graph.keys 
      } else {
        val degrees = graph.map(p => (p._1, p._2.size))
        val sampleIters = quickSample(degrees, epsilonP)
        val rhoHat = density + 8 * math.log(nNodes) / epsilon + Lap(2 * math.log(nNodes)/(graph.size * epsilon))
        val T_i = math.exp(2 * epsilonP * rhoHat) * 2 * math.log(nNodes)

        println("laplace mag", 2 * math.log(nNodes)/(graph.size * epsilonP))
        //;; for (i <- 1 to 100 ) {
        //;;   println("laplace", Lap(4 * math.log(nNodes)/(graph.size * epsilonP)))
        //;; }
        println("phase", phaseI, "node", nNodes, density, rhoHat, T_i)

        // Select nodes to remove at this phase
        var randomNodes = graph.filter(p => (sampleIters(p._1) <= T_i))

        while (randomNodes.size != 0) {
          // Next iter to remove node
          val (minIterIndex, minIterToRemove) = randomNodes.minBy(p => sampleIters(p._1))

          densitySubGraphs += density

          val toRemovedNodes = randomNodes.filter(p => p._2 == minIterToRemove)

          removedNodes += toRemovedNodes.keySet

          randomNodes = randomNodes.filterNot(p => p._2 == minIterToRemove)

          toRemovedNodes.foreach(p => p._2.foreach(pp => graph.getOrElse(pp, Set()).remove(p._1)))
          graph --= toRemovedNodes.keys
          //        println(graph.find(p => (sampleIters(p._1) == minIterToRemove)))
          //        println(minIterIndex + " minIter: " + sampleIters(minIterIndex))
          //        println("Trigger quick sample at iter " + countIter + " ,minIterToRemove: " + minIterToRemove + " removeSizes: " + randomNodes.size)
        }
      }
      phaseI += 1
    }

    val sampledSub = utils.sampleRho(densitySubGraphs, epsilon/4)
    
    removedNodes.slice(0, sampledSub._2).foreach(nodes --= _)
    
    return (sampledSub._1, nodes, phaseI)
  }
}
