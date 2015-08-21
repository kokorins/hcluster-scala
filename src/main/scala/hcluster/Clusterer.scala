package hcluster

import Types._

import scala.collection.immutable

// TODO Support minSimilarity = 0d
trait Clusterer[A] {
  this: PairGenerator with SimilarityMetric[A] with ClusterEvaluator =>
  def cluster(values: IndexedSeq[A]): (Score, Seq[Seq[A]]) = {
    val pairs: IndexedSeq[(Index, Index)] = pairsFrom(values)
    def doCompare(i: Index, j: Index) = compare(values(i), values(j))

    val similarityMatrix: SimilarityMatrix = SimilarityMatrix(doCompare, pairs, lowThreshold)
    val seedClusters: immutable.IndexedSeq[Cluster] = values.indices.map(Cluster(_))
    val (evaluation: Score, clusters: Seq[Cluster]) =
      agglomerate(seedClusters, (evaluate(seedClusters, similarityMatrix), seedClusters), similarityMatrix)

    (evaluation, clusters.map(_.elements map values))
  }

  private def agglomerate(clusters: Seq[Cluster],
                          bestSoFar: (Score, Seq[Cluster]),
                          similarityMatrix: SimilarityMatrix): (Score, Seq[Cluster]) = {

    val (bestScoreSoFar: Score, bestClustersSoFar: Seq[Cluster]) = bestSoFar

    val (newScore: Score, newClusters: Seq[Cluster]) = cluster(clusters, similarityMatrix)

    if (newClusters.length == clusters.length)
      (bestScoreSoFar, bestClustersSoFar)
    else {
      val nextBestSoFar: (Score, Seq[Cluster]) =
        if (isScoreBetter(bestScoreSoFar, newScore))
          (bestScoreSoFar, bestClustersSoFar)
        else
          (newScore, newClusters)

      agglomerate(newClusters, nextBestSoFar, similarityMatrix)
    }
  }

  private def cluster(clusters: Seq[Cluster], similarityMatrix: SimilarityMatrix): (Score, Seq[Cluster]) = {
    val dendrogram: Dendrogram = Dendrogram(clusters, similarityMatrix)

    val thresholds: Seq[Similarity] = Dendrogram.thresholds(dendrogram) filter (_ > lowThreshold)

    if (thresholds.isEmpty)
      (evaluate(clusters, similarityMatrix), clusters)
    else {
      val evaluations: Seq[(Score, IndexedSeq[Cluster])] =  thresholds map { threshold =>
        val cuts: IndexedSeq[Dendrogram] = dendrogram cutAt threshold
        val clusters: IndexedSeq[Cluster] =  cuts map (Cluster(_, similarityMatrix))
        (evaluate(clusters, similarityMatrix), clusters)
      }

      val bestScore: Score = selectBestScore(evaluations.map(_._1))

      val bestClusters: (Score, IndexedSeq[Cluster]) = evaluations.find(_._1 == bestScore).get

      bestClusters
    }
  }
}
