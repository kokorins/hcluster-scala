package hcluster

import Types._

trait ClusterEvaluator {
  def evaluate(clusterPartition: Seq[Cluster], similarityMatrix: SimilarityMatrix): Score
  def isScoreBetter(s1: Score, s2: Score): Boolean

  def selectBestScore(scores: Seq[Score]): Score = scores match {
    case Nil => 0d
    case _ =>
      scores.tail.foldLeft(scores.head) { (bestSoFar, currentScore) =>
        if (isScoreBetter(bestSoFar, currentScore))
          bestSoFar
        else
          currentScore
      }
  }
}

trait MinimizingClusterEvaluator extends ClusterEvaluator {
  def isScoreBetter(s1: Score, s2: Score): Boolean = s1 < s2
}
trait MaximizingClusterEvaluator extends ClusterEvaluator {
  def isScoreBetter(s1: Score, s2: Score): Boolean = s1 > s2
}

trait MaxIntraSimilarityClusterEvaluator extends MaximizingClusterEvaluator {
  def evaluate(clusterPartition: Seq[Cluster], similarityMatrix: SimilarityMatrix): Score = {
    if (clusterPartition.isEmpty)
      0d
    else
      clusterPartition.map(_.intraSimilarity).sum / clusterPartition.length
  }
}

trait DaviesBouldinClusterEvaluator extends MinimizingClusterEvaluator {
  def evaluate(clusterPartition: Seq[Cluster], similarityMatrix: SimilarityMatrix): Score = {
    val scores =
      for (i <- clusterPartition.indices; j <- i + 1 until clusterPartition.length) yield {
        val c1 = clusterPartition(i)
        val c2 = clusterPartition(j)
        (c1.intraSimilarity + c2.intraSimilarity) / similarityMatrix(c1.centroid, c2.centroid)
      }

    print(s"${clusterPartition.length}: ${scores.length} -> ")
    val daviesBouldin =
      if (scores.length < 2 || scores.max == 0d) Double.MaxValue
      else scores.max / clusterPartition.length
    println(s"$daviesBouldin")

    daviesBouldin
  }
}
