package hcluster

import hcluster.Types._
trait DendrogramEvaluator {
  def similarity(lhs:Dendrogram, rhs:Dendrogram, similarityMatrix: SimilarityMatrix):Similarity
}

trait MaxElementDendrogramEvaluator extends DendrogramEvaluator {
  def similarity(lhs:Dendrogram, rhs:Dendrogram, similarityMatrix: SimilarityMatrix) = {
    val similarities = for {
      leftCluster <- lhs.clusters
      rightCluster <- rhs.clusters
    } yield (for {i <- leftCluster.elements; j <- rightCluster.elements} yield similarityMatrix(i, j)).max

    similarities.max
  }
}

trait Dendrogramer {
  this: DendrogramEvaluator =>
  /**
   * Calculates the dendrogram structure with a given similarity matrix of initial points
   * @param similarityMatrix matric of elements similarity
   * @return the dendrogram of agglomerating process
   */
  def calc(similarityMatrix: SimilarityMatrix): Dendrogram = {
    val seedPairs = for (i <- 0 until similarityMatrix.size) yield (i, Leaf(Cluster(i)))
    val seedMap = Map(seedPairs: _*)
    val dendrograms = agglomerate(seedMap, similarityMatrix)
    dendrograms(0)
  }

  def agglomerate(dendrograms: Map[Int, Dendrogram], similarityMatrix: SimilarityMatrix): Map[Int, Dendrogram] = {
    def merge(dendrograms: Map[Int, Dendrogram]) = {
      val dendrogramVector = dendrograms.toVector
      val pairs = for {i <- dendrogramVector.indices; j <- i + 1 until dendrogramVector.length}
        yield (i, j)

      def similarityTriplet(pair: (Index, Index)): (Index, Index, Similarity) = {
        val (i: Index, j: Index) = pair
        val (leftIndex: Index, leftDendrogram: Dendrogram) = dendrogramVector(i)
        val (rightIndex: Index, rightDendrogram: Dendrogram) = dendrogramVector(j)
        (leftIndex, rightIndex, similarity(leftDendrogram, rightDendrogram, similarityMatrix))
      }

      val similarityTriplets: IndexedSeq[(Index, Index, Similarity)] = (pairs map similarityTriplet) sortBy (_._3)
      val (i: Index, j: Index, similarityValue: Similarity) = similarityTriplets.head
      dendrograms - i - j + (Math.min(i, j) -> Branch(similarityValue, dendrograms(i), dendrograms(j)))
    }
    if (dendrograms.size == 1)
      dendrograms
    else
      agglomerate(merge(dendrograms), similarityMatrix)
  }
}
