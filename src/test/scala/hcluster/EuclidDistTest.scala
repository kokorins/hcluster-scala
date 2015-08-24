package hcluster

import org.scalatest.FunSuite

class EuclidDistTest extends FunSuite {
  test("Euclid distance smoke") {
    val input = Seq(List(1.0), List(2.0), List(-1.0), List(-2.0))
    val pairs = for {
      i <- input.indices
      j <- i + 1 until input.length
    } yield (i, j)

    val ed = new EuclidSimilarityMetric {}
    val compare = (i: Int, j: Int) => ed.compare(input(i), input(j))
    val similarityMatrix = SimilarityMatrix(compare, pairs, 0d)
    val dendrogramer = new Dendrogramer with MaxElementDendrogramEvaluator ()
    System.out.println(Dendrogram.thresholds(dendrogramer.calc(similarityMatrix)))
  }
}
