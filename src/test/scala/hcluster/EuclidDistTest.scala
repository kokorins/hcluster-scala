package hcluster

import hcluster.Types.{Score, Similarity}
import org.scalatest.FunSuite

class EuclidDistTest extends FunSuite {
  test("Euclid distance smoke") {
    val clusterer = new Clusterer[List[Double]] with EuclidSimilarityMetric with ExahaustivePairGenerator with MaximizingClusterEvaluator
    {
      override val lowThreshold: Similarity = 0.0

      override def evaluate(clusterPartition: Seq[Cluster], similarityMatrix: SimilarityMatrix): Score = {
        val indexedClusterPartition = clusterPartition.zipWithIndex
        var min = Double.MaxValue
        for(lhs<-indexedClusterPartition; rhs<-indexedClusterPartition if lhs._2<rhs._2) {
          val l = lhs._1
          val r = rhs._1
          for(lIdx <- l.elements; rIdx <- r.elements) {
            val cur = similarityMatrix(lIdx, rIdx)
            if(min>cur)
              min = cur
          }
        }
        min
      }
    }
    val input = Seq(List(1.0), List(2.0), List(-1.0), List(-2.0))
    val res = clusterer.cluster(input.toIndexedSeq)
    System.out.println(res)
    val pairs = for {
      i <- input.indices
      j <- i + 1 until input.length
    } yield (i, j)

    val ed = new EuclidSimilarityMetric {}
    val compare = (i: Int, j: Int) => ed.compare(input(i), input(j))
    val similarityMatrix = SimilarityMatrix(compare, pairs, 0d)
    val dendrogram = Dendrogram(input.size, similarityMatrix)
    System.out.println(Dendrogram.thresholds(dendrogram))
  }
}
