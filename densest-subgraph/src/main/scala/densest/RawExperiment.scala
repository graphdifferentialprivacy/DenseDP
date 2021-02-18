package densest

class RawExperiment(
    var network: String,
    var networkSize: Int,
    var algo: String,
    var epsilon: Double,
    var delta: Double,
    var density: Double,
    var subgraph: Set[Int]
    ) {
  
    override def toString: String =
      s"$network, $networkSize, $algo, $epsilon, $delta,  $density, $subgraph"
}