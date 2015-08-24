package hcluster

import Types._

trait PairGenerator {
  def pairsFrom[A](elements: IndexedSeq[A]): IndexedSeq[(Index, Index)]
}

// TODO Add unit tests for ExhaustivePairGenerator
trait ExhaustivePairGenerator extends PairGenerator {
  def pairsFrom[A](elements: IndexedSeq[A]) = ExhaustivePairGenerator.pairsFrom(elements.length)
}
object ExhaustivePairGenerator {
  def pairsFrom(length: Int): IndexedSeq[(Index, Index)] =
    for (i <- 0 until length; j <- i + 1 until length)
      yield (i, j)
}