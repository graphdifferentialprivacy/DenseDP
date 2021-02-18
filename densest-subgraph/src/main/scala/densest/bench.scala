package densest

import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.Map
import scala.collection.mutable.Set
import scala.collection.parallel.ForkJoinTaskSupport
import java.io.FileOutputStream
import java.io.PrintWriter
import java.io.File

object bench {
  val DATA_DIR = "data/"
  val OUTPUT_DIR = "output_phase"
  val OUTPUT_FILE = "bench_accuracy"
  val EXPR_REPEAT = 10
  val EXPR_REPEAT_TEST = 2
  val NUM_THREADS = 40
  val NUM_THREADS_TEST = 2
  val TEST_KEYWORD = "test"

  val NETWORKS: Array[Array[String]] = Array(
    Array( // 0
      "ca-GrQc",
      "ca-HepTh",
    ),
    Array( // 1
      "lastfm_asia_edges",
      "deezer_europe_edges",
    ),
    Array( // 2
      "musae_squirrel_edges",
      "musae_crocodile_edges",
      "musae_chameleon_edges",
      "musae_PTBR_edges",
      "musae_DE_edges",
      "musae_ENGB_edges",
      "musae_facebook_edges",
      "musae_FR_edges",
      "musae_git_edges",
    ),
    Array( // 3
      "ca-HepPh"
    ),
    Array( // 4
      "loc-brightkite_edges"
    ),
    Array( // 5
      "email-Enron"
    ),
    Array( // 6
      "ca-CondMat",
      "ca-AstroPh"
    ),
    Array( // 7
      "facebook_combined"
    ),
    Array( // 8
      "loc-gowalla_edges",
    ),
    Array( // 9
      "roadNet-CA",
      "roadNet-PA",
      "roadNet-TX"
    )
  )

  val EPSILONS: Array[Double] = Array(0.25, 0.5, 1, 2, 4, 8, 16)
  //val EPSILONS: Array[Double] = Array(0.1, 0.5, 1, 1.5, 2, 2.5, 3, 3.5, 4)

  val DELTAS: Array[Double] = Array(math.pow(10, -6), math.pow(10, -9))

  def main(args: Array[String]): Unit = {

    val configs: ArrayBuffer[(String, Double, Double)] = ArrayBuffer()

    var repeat = EXPR_REPEAT

    var threads = NUM_THREADS

    if (args.size != 0 && !args(0).equalsIgnoreCase(TEST_KEYWORD)) {
      val netGroup = args(0).toInt
      println("Run group: " + netGroup)
      for (net <- NETWORKS(netGroup))
        for (eps <- EPSILONS)
          for (delta <- DELTAS)
            configs += ((net, eps, delta))
    } else {
      println("Test")
      for (net <- NETWORKS(0).slice(0, 1))
        for (eps <- EPSILONS.slice(4, 6))
          for (delta <- DELTAS.slice(0, 1))
            configs += ((net, eps, delta))
      repeat = EXPR_REPEAT_TEST
      threads = NUM_THREADS_TEST
    }

    val directory = new File(OUTPUT_DIR);
    if (!directory.exists()) {
      directory.mkdirs();
      // If you require it to make the entire directory path including parents,
      // use directory.mkdirs(); here instead.
    }

    val outputFile = OUTPUT_DIR + "/" + OUTPUT_FILE + "_" + args(0) + ".txt"

    val pw = new PrintWriter(new FileOutputStream(new File(outputFile)), true)

    val cfgPar = configs.par
    cfgPar.tasksupport = new ForkJoinTaskSupport(new java.util.concurrent.ForkJoinPool(threads))

    cfgPar.foreach(p =>
      { pw.println(bench_network(p._1, p._2, p._3, repeat).toString().replaceAll("[\\(\\)]", "")); pw.flush() })

    pw.close
  }

  def bench_network(
    network: String,
    epsilon: Double,
    delta:   Double,
    repeat:  Int): SummaryOutput = {
    val graph = utils.loadGraph(DATA_DIR + network + ".txt")

    val (baseOutput, baseSubgraph, _) = baseline.baseline(utils.cloneGraph(graph))

    val (seqOutputs, seqSubGraphs, _) = bench_method(
      graph,
      phase.parPhase,
      //sequential.privateDensest,
      epsilon,
      delta,
      repeat)

    val (seqMean, seqStd) = summary(seqOutputs)

    val (parOutputs, parSubGraphs, parIters) = bench_method(
      graph,
      parallel.prvtDensParQuick,
      epsilon,
      delta,
      repeat)

    val (parMean, parStd) = summary(parOutputs)
    val (iterMean, iterStd) = summary(parIters)
    val (seqSizeMean, seqSizeStd) = summary(seqSubGraphs.map(_.size.toDouble))
    val (parSizeMean, parSizeStd) = summary(parSubGraphs.map(_.size.toDouble))
    val (parJacMean, parJacStd) = summary(jaccard(baseSubgraph, parSubGraphs))
    val (seqJacMean, seqJacStd) = summary(jaccard(baseSubgraph, seqSubGraphs))
    val (parRecMean, parRecStd) = summary(recall(baseSubgraph, parSubGraphs))
    val (seqRecMean, seqRecStd) = summary(recall(baseSubgraph, seqSubGraphs))


    return new SummaryOutput(network, graph.size, graph.map(p => p._2.size).sum/2,
      epsilon, delta,
      baseOutput, baseSubgraph.size,
      seqMean, seqStd, seqSizeMean, seqSizeStd, seqJacMean, seqJacStd, seqRecMean, seqRecStd,
      parMean, parStd, parSizeMean, parSizeStd, parJacMean, parJacStd, parRecMean, parRecStd,
      iterMean, iterStd)
  }

  def bench_method(
    graph:   Map[Int, Set[Int]],
    alg:     (Map[Int, Set[Int]], Double, Double) => (Double, scala.collection.Set[Int], Int),
    epsilon: Double,
    delta:   Double,
    repeat:  Int): (ArrayBuffer[Double], ArrayBuffer[scala.collection.Set[Int]], ArrayBuffer[Double]) = {
    val outputs: ArrayBuffer[Double] = ArrayBuffer()
    val iters: ArrayBuffer[Double] = ArrayBuffer()
    val subgraphs: ArrayBuffer[scala.collection.Set[Int]] = ArrayBuffer()

    for (i <- 1 to repeat) {
      val (output, subgraph, iter) = alg(utils.cloneGraph(graph), epsilon, delta)
      outputs += output
      subgraphs += subgraph
      iters += iter.toDouble
    }

    return (outputs, subgraphs, iters)
  }

  def summary(array: ArrayBuffer[Double]): (Double, Double) = {
    val mean = array.sum / array.size
    val std = math.sqrt(array.map(p => math.pow(p - mean, 2)).sum / (array.size - 1))

    return (mean, std)
  }

  def jaccard(baseOutput: scala.collection.Set[Int], outputs: ArrayBuffer[scala.collection.Set[Int]]): ArrayBuffer[Double] =
  {
    val jaccards: ArrayBuffer[Double] = ArrayBuffer()

    outputs.foreach(jaccards += jaccard(baseOutput, _))

    return jaccards
  }

  def jaccard(baseOutput: scala.collection.Set[Int], output: scala.collection.Set[Int]): Double =
  {
    if (baseOutput.size == 0 && output.size == 0)
      return 1.0

    val intersect = baseOutput & output
    return intersect.size.toDouble / (baseOutput.size + output.size - intersect.size)
  }

  def recall(baseOutput: scala.collection.Set[Int], outputs: ArrayBuffer[scala.collection.Set[Int]]): ArrayBuffer[Double] =
  {
    val jaccards: ArrayBuffer[Double] = ArrayBuffer()

    outputs.foreach(jaccards += recall(baseOutput, _))

    return jaccards
  }

  def recall(baseOutput: scala.collection.Set[Int], output: scala.collection.Set[Int]): Double =
  {
    if (baseOutput.size == 0 && output.size == 0)
      return 1.0

    val intersect = baseOutput & output
    return intersect.size.toDouble / (baseOutput.size)
  }
}
