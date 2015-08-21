package hcluster

import Types._

import scala.collection.immutable

/**
 * Class that represents the group of points
 * @param centroid index of a input element that supposed to be centroid
 * @param intraSimilarity score of similarity of points inside the cluster, which calculates as an average distance from point to centroid point
 * @param elements indices of elements groupped in cluster
 */
case class Cluster(centroid: Index, intraSimilarity: Similarity, elements: Seq[Int]) {

  /**
   * Distance to the other cluster
   * @param other the other cluster
   * @param similarityMatrix distances between all the input data points
   * @return the score of cluster similarity
   */
  def similarity(other: Cluster, similarityMatrix: SimilarityMatrix): Similarity =
    Cluster.similarity(this, other, similarityMatrix)

  def toString[A](as: IndexedSeq[A]) =
    s"centroid: ${as(centroid)}, intraSimilarity: $intraSimilarity, elements: ${(elements map as) mkString(",")}"
}

object Cluster {

  /**
   * Create cluster from the input element index
   * @param index to be the centroid of cluster
   * @return newly created cluster
   */
  def apply(index: Index): Cluster = new Cluster(index, 0d, Vector(index))

  /**
   * joins the elements of all the clusters in dendrogram into one cluster, selects new centroid and
   * calculates the intraSimilarity as distance to the centroid
   * @param dendrogram dendrogram of elements joined into one cluster
   * @param similarityMatrix matrix of distances between each points of input
   * @return newly created cluster
   */
  def apply(dendrogram: Dendrogram, similarityMatrix: SimilarityMatrix): Cluster = {
    val elements: IndexedSeq[Index] = dendrogram.clusters flatMap (_.elements)
    val (centroid: Index, intraSimilarity: Similarity) = centroidPair(elements, similarityMatrix)
    new Cluster(centroid, intraSimilarity, elements)
  }

  /**
   * Do centroid selection by means of sorting the average distances from the given node to all other nodes
   * @param elements groupped in current cluster
   * @param similarityMatrix distances between all input elements
   * @return index of element first with highest average similarity
   */
  def centroidPair(elements: Seq[Int], similarityMatrix: SimilarityMatrix): (Index, Similarity) = {
    val similarityTriplets: IndexedSeq[(Index, Index, Similarity)] =
      for (i <- elements.indices;
           j <- i + 1 until elements.length)
        yield (elements(i), elements(j), similarityMatrix(elements(i), elements(j)))

    val similarityPairs: Seq[(Index, Similarity)] =
      ((similarityTriplets ++ (similarityTriplets map (t => (t._2, t._1, t._3)))) groupBy {case (left: Index, right: Index, _) =>
          left
      } map { case(i: Index, t: IndexedSeq[(Index, Index, Similarity)]) =>
          (i, t.map(_._3).sum / t.length)
      }).toSeq

    val orderedPairs: Seq[(Index, Similarity)] = similarityPairs sortBy(-_._2)

    orderedPairs.headOption getOrElse (elements head, 0d)
  }

  /**
   * Similarity between clusters is calculated as distance between their centroid elements
   * @param lhs left cluster
   * @param rhs right cluster
   * @param similarityMatrix input elements distances
   * @return similarity between clusters
   */
  def similarity(lhs: Cluster, rhs: Cluster, similarityMatrix: SimilarityMatrix): Similarity = {
    similarityMatrix(lhs.centroid, rhs.centroid)
//    val similarities =
//      for (i <- 0 until c1.elements.length; j <- 0 until c2.elements.length)
//      yield similarityMatrix(i, j)
//    if (similarities.length == 0) 0d
//    else similarities.sum / similarities.length
  }
}

