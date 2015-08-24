package hcluster

import Types._

import scala.collection.parallel.ParSeq

trait SimilarityMatrix {
  def size: Index
  protected[SimilarityMatrix] def map: Map[Int, Map[Int, Similarity]]

  def apply(i: Index, j: Index): Similarity = {
    validate(i); validate(j)

    if (i == j) 1d
    else {
      val (l: Index, r: Index) = SimilarityMatrix.orderedIndexes(i, j)
      map.getOrElse(l, Map.empty).getOrElse(r, 0d)
    }
  }

  def validate(index: Index) =
    if (!(index >= 0 && index < size))
      throw new IllegalArgumentException(s"Invalid index not between 0 and ${size - 1}:  $index")

  override def toString = s"$size: ${map.toString()}"
}

object SimilarityMatrix {
  def apply(compare: (Index, Index) => Similarity,
            pairs: Seq[(Index, Index)],
            minThreshold: Similarity = 0d): SimilarityMatrix =
  {
    val triplets: ParSeq[(Index, Index, Similarity)] =
      pairs.par.map { case (i, j) =>
        (i, j, compare(i, j))
      }

    def addPair(accum: (Index, Map[Int, Map[Int, Similarity]]), triplet: (Index, Index, Similarity)): (Index, Map[Int, Map[Int, Similarity]]) = {
      val (maxIndex: Index, map: Map[Index, Map[Index, Similarity]]) = accum
      val (leftIndex: Index, rightIndex: Index, similarity: Similarity) = triplet

      val pairMax = math.max(leftIndex, rightIndex)
      val newMaxIndex = if (pairMax > maxIndex) pairMax else maxIndex

      if (similarity <= minThreshold) (newMaxIndex, map)
      else {
        val (i: Index, j: Index) = orderedIndexes(leftIndex, rightIndex)
        (newMaxIndex, map + (i -> (map.getOrElse(i, Map.empty) + (j -> similarity))))
      }
    }

    val (maxIndex: Index, similarityMap: Map[Index, Map[Index, Similarity]]) =
      triplets.seq.foldLeft(0, Map[Int, Map[Int, Similarity]]())(addPair)

    new SimilarityMatrix {
      val map: Map[Index, Map[Index, Similarity]] = similarityMap
      val size: Index = maxIndex + 1
    }
  }

  def orderedIndexes(i: Index, j: Index): (Index, Index) = {
    val l: Index = math.min(i, j)
    val r: Index = math.max(i, j)
    (l, r)
  }
}
