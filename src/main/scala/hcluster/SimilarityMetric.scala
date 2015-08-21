package hcluster

import Types._

trait SimilarityMetric[A] {
  def lowThreshold: Similarity = 0d
  def compare(lhs: A, rhs: A): Similarity
}

trait LuceneSimilarityMetric extends SimilarityMetric[String] {
  import org.apache.lucene.search.spell.StringDistance

  def distance: StringDistance
  def compare(lhs: String, rhs: String) = distance.getDistance(lhs, rhs).toDouble
}

trait EuclidSimilarityMetric extends SimilarityMetric[List[Double]] {
  def compare(lhs: List[Double], rhs: List[Double]) = (lhs zip rhs).aggregate(0.0)((ss, p)=>ss+Math.pow(p._1-p._2,2), _+_)
}
