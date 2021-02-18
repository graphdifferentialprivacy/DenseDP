package densest

class SummaryOutput( 
 var network: String,
 var graphSize: Int,
 var edges: Int,
 var epsilon: Double,
 var delta: Double,

 var baseOutput:Double,
 var baseOutputSize:Double,

 var seqMean: Double,

 var seqStd: Double,
 var seqSizeMean: Double,
 var seqSizeStd: Double,
 var seqJacMean: Double,
 var seqJacStd: Double,
 var seqRecMean: Double,
 var seqRecStd: Double,

 var parMean: Double,
 var parStd: Double,
 var parSizeMean: Double,
 var parSizeStd: Double,
 var parJacMean: Double,
 var parJacStd: Double,
 var parRecMean: Double,
 var parRecStd: Double,
 var iterMean: Double,
 var iterStd: Double,
) {
  override def toString: String =
    s"$network, $graphSize, $edges, $epsilon, $delta, $baseOutput, $baseOutputSize, " +
    s"$seqMean, $seqStd, $seqSizeMean, $seqSizeStd, $seqJacMean, $seqJacStd, $seqRecMean, $seqRecStd, " +
    s"$parMean, $parStd, $parSizeMean, $parSizeStd, $parJacMean, $parJacStd, $parRecMean, $parRecStd, $iterMean, $iterStd"
}
