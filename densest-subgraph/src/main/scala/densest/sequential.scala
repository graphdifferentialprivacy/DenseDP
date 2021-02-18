package densest

import scala.collection.mutable.Map
import scala.collection.mutable.Set
import scala.collection.mutable.ArrayBuffer
import scala.util.Random

object sequential {
  def main(args: Array[String]): Unit = {
    val graph = utils.loadGraph("data/musae_chameleon_edges.txt")
    
    val outputs : ArrayBuffer[Double] = ArrayBuffer()
    val runtimes : ArrayBuffer[Long] = ArrayBuffer()    

    for (i <- 1 to 10) {
      val start = System.currentTimeMillis()
      outputs += privateDensest(utils.cloneGraph(graph), 1, math.pow(10, -6))._1
      val end = System.currentTimeMillis()
      runtimes += (end - start) 
    }
    
    val mean = outputs.sum / outputs.size
    println("Mean: " + mean)
    println("Std: " + math.sqrt(outputs.map(p => math.pow(p - mean, 2)).sum / (outputs.size-1)))
    println(s"Runtime: $runtimes")
  }
  
  def privateDensest(graph: Map[Int, Set[Int]], epsilon: Double, delta: Double): (Double, scala.collection.Set[Int], Int) = {
    val nNodes = graph size
    val densitySubGraphs: ArrayBuffer[Double] = ArrayBuffer()
    val subGraph = graph.clone
    val privacyRatio = 0.9
    //val sampleRatio = 0.1
    
    val epsilonP = epsilon / (2 * math.log(math.E/delta)) * privacyRatio

    val removedNodes: ArrayBuffer[Int] = ArrayBuffer() 
    // while loop execution
    while (subGraph.size > 0) {
      val density = utils.rho(subGraph)
      densitySubGraphs += density
      //println(density)
      
      // Randomize some node with probability proportional to -degree 
      val randomNode =  sampleNode(subGraph, epsilonP)
      
      // Reconstruct the adjacent list:
      randomNode._2.foreach(p => subGraph.getOrElse(p, Set()).remove(randomNode._1))

      // Remove the min degree node
      subGraph.remove(randomNode._1)
      removedNodes += randomNode._1
       
    }

    val sampledSub = utils.sampleRho(densitySubGraphs, epsilon * (1-privacyRatio))
    // sample 10%
    //val sampledSub = utils.sampleRho(densitySubGraphs.filter(p => Random.nextDouble < sampleRatio), epsilon * (1-privacyRatio)/sampleRatio)
    // println(s"Density subgraphs $densitySubGraphs")
    return (sampledSub._1, graph.keySet -- removedNodes.slice(0, sampledSub._2), nNodes)
  }


  // https://stackoverflow.com/questions/24869304/scala-how-can-i-generate-numbers-according-to-an-expected-distribution
  final def sampleNode(dist: Map[Int, Set[Int]], epsilon: Double): (Int, Set[Int]) = {
    val p = Random.nextDouble * dist.map(p => math.exp(-epsilon * p._2.size)).sum
    val iter = dist.iterator
    var accumulative = 0.0
    while (iter.hasNext) {
      val (node, adjList) = iter.next
      accumulative += math.exp(-epsilon * adjList.size)
      if (accumulative >= p)
        return (node, adjList)
    }
    sys.error(f"Distribution error") // needed so it will compile
  }

}
