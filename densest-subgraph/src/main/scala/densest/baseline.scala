package densest

import scala.collection.mutable.Map
import scala.collection.mutable.Set
import scala.collection.mutable.ArrayBuffer
import scala.collection.parallel.mutable.ParMap
import scala.collection.parallel.mutable.ParHashMap

object baseline {
  def main(args: Array[String]):Unit = {
    val graph = utils.loadGraph("data/musae_chameleon_edges.txt")
    
    val graphPar = utils.cloneGraph(graph)

    val start = System.currentTimeMillis()
    val densest = baseline(graphPar)
    val end = System.currentTimeMillis()
    val denseSub = densest._2
    
    println(s"Baseline Densest subgraph: $densest")
    println(s"Densest sub: $denseSub")
    println("Densest size: " + denseSub.size)
    println("Runtime: " + (end - start))
    println("Density: " + densest._1)
    
    return 
  }

  def baseline(graph: Map[Int, Set[Int]]): (Double, scala.collection.Set[Int], Int) = {
    val nNodes = graph size
    val densitySubGraphs: ArrayBuffer[Double] = ArrayBuffer()
    val nodes: Set[Int] = Set()
    graph.keySet.foreach(p => nodes.add(p))
    
    val removedNodes: ArrayBuffer[Int] = ArrayBuffer() 

    // while loop execution
    while (graph.size > 0) {
      // Calculate density of current subgraph
      val density = utils.rho(graph)
      densitySubGraphs += density
      //println(s"Desity: $density Subgraph: $subGraph")
      
      // Find the min degree node
      val minDegNode = graph.minBy(p => p._2.size)
      
      // Reconstruct the adjacent list:
      minDegNode._2.foreach(p => graph.getOrElse(p, Set()).remove(minDegNode._1))

      // Remove the min degree node
      graph.remove(minDegNode._1)
      removedNodes += minDegNode._1
      
    }

    val maxDensity = densitySubGraphs.max
    val maxDensityStep = densitySubGraphs.indexWhere(_ >= maxDensity)
    return (maxDensity, nodes -- removedNodes.slice(0, maxDensityStep + 1), nNodes) 
  }

}
