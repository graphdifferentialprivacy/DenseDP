package densest

import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.Map
import scala.collection.mutable.Set
import scala.collection.mutable.ArrayBuffer
import scala.util.Random

object parallel {
  
  def main(args: Array[String]): Unit = {
    val graph = utils.loadGraph("ca-GrQc.txt")
    
    val outputs : ArrayBuffer[Double] = ArrayBuffer()
    val iters: ArrayBuffer[Int] = ArrayBuffer()
    
    for (i <- 1 to 10) {
      val (density, subgraph, iter) = prvtDensParQuick(utils.cloneGraph(graph), 1, math.pow(10, -9))
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

  def prvtDensPar(graph: Map[Int, Set[Int]], epsilon: Double, delta: Double): (Double, scala.collection.Set[Int], Int) = {
    val nNodes = graph size
    val densitySubGraphs: ArrayBuffer[Double] = ArrayBuffer()

    val nodes: Set[Int] = Set()
    graph.keySet.foreach(p => nodes.add(p))

    val epsilonP = epsilon * (1 - 1/math.E) / (8 * math.log(math.E/delta))

    val removedNodes: ArrayBuffer[scala.collection.Set[Int]] = ArrayBuffer() 

    var countIter = 0
    // while loop execution
    while (graph.size > 0) {
      countIter += 1
      
      // Optimize here: sample until there is boolean true
      val randomNodes = graph.filter(p => sample(p._2.size, epsilonP))
      
      if (randomNodes.size != 0) {
        val density = utils.rho(graph)
        densitySubGraphs += density

        // Reconstruct the adjacent list:
        randomNodes.foreach(p => p._2.foreach(pp => graph.getOrElse(pp, Set()).remove(p._1)))

        // Remove the min degree node
        // TODO keys or keyset?
        graph --= randomNodes.keys
        removedNodes += randomNodes.keySet
      }
    }

    val sampledSub = utils.sampleRho(densitySubGraphs, epsilon/2)
    
    removedNodes.slice(0, sampledSub._2).foreach(nodes --= _)
    
    return (sampledSub._1, nodes, countIter)
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
  
  def prvtDensParQuick(graph: Map[Int, Set[Int]], epsilon: Double, delta: Double): (Double, scala.collection.Set[Int], Int) = {
    val nNodes = graph size
    val densitySubGraphs: ArrayBuffer[Double] = ArrayBuffer()

    val nodes: Set[Int] = Set()
    graph.keySet.foreach(p => nodes.add(p))

    val epsilonP = epsilon * (1 - 1/math.E) / (8 * math.log(math.E/delta))

    val removedNodes: ArrayBuffer[scala.collection.Set[Int]] = ArrayBuffer() 

    var countIter = 0
    var lastRemoved = 0
    // while loop execution
    while (graph.size > 0) {
      countIter += 1
      
      // Optimize here: sample until there is boolean true
      var randomNodes = graph.filter(p => sample(p._2.size, epsilonP))
      
      if (randomNodes.size == 0 & countIter - lastRemoved >= 100) {
        val degrees = graph.map(p => (p._1, p._2.size))
        val sampleIters = quickSample(degrees, epsilonP)
        val (minIterIndex, minIterToRemove) = sampleIters.minBy(p => p._2)
        randomNodes = graph.filter(p => (sampleIters(p._1) == minIterToRemove))
        countIter += (minIterToRemove)
//        println(graph.find(p => (sampleIters(p._1) == minIterToRemove)))
//        println(minIterIndex + " minIter: " + sampleIters(minIterIndex))
//        println("Trigger quick sample at iter " + countIter + " ,minIterToRemove: " + minIterToRemove + " removeSizes: " + randomNodes.size)
      }

      if (randomNodes.size != 0) {
        val density = utils.rho(graph)
        densitySubGraphs += density

        // Reconstruct the adjacent list:
        randomNodes.foreach(p => p._2.foreach(pp => graph.getOrElse(pp, Set()).remove(p._1)))

        // Remove the min degree node
        // TODO keys or keyset?
        graph --= randomNodes.keys
        removedNodes += randomNodes.keySet
        lastRemoved = countIter
      } 
    }

    val sampledSub = utils.sampleRho(densitySubGraphs, epsilon/2)
    
    removedNodes.slice(0, sampledSub._2).foreach(nodes --= _)
    
    return (sampledSub._1, nodes, countIter)
  }
}
