package hcluster

import org.scalatest.FunSuite
import com.typesafe.scalalogging.Logging
import org.apache.lucene.search.spell.JaroWinklerDistance
import org.slf4j.LoggerFactory
import com.typesafe.scalalogging.slf4j.Logger
import java.io.{File, FileWriter, PrintWriter}

class DendrogramTest extends FunSuite with Logging {
  val logger = Logger(LoggerFactory getLogger "DendrogramTest")

  val names = Vector(
    "alejandro", "alejito", "alejo",
    "malrene", "marlen", "marlene", "marleny",
    "marta", "martha",
    "ricardo")

  val pairs = for {
    i <- 0 until names.length
    j <- i + 1 until names.length
  } yield (i, j)

  val minSimilarity = 0d
  val jaroWinkler = new JaroWinklerDistance
  val compare = (i: Int, j: Int) => jaroWinkler.getDistance(names(i), names(j)).toDouble

  val similarityMatrix = SimilarityMatrix(compare, pairs, minSimilarity)

  val dendrogram = Dendrogram(names.length, similarityMatrix)

  val expectedThresholds = Seq(
    0.5587742576996485, 0.5886243581771851,
    0.6447619050741196, 0.6767262145876884,
    0.879365086555481, 0.9428572058677673,
    0.9555555582046509, 0.9666666388511658, 0.9809523820877075)

  test("Creates proper dendrogram") {
    assert(Dendrogram.thresholds(dendrogram) == expectedThresholds)
  }

  test("Cuts properly at threshold") {
    val expectedCutSizes = Seq(
      // 0.5587742576996485
        Seq(10),
      // 0.5886243581771851
        Seq(4, 6),
      // 0.6447619050741196
        Seq(4, 6),
      // 0.6767262145876884
        Seq(2, 1, 1, 6),
      // 0.879365086555481
        Seq(2, 1, 1, 4, 2),
      // 0.9428572058677673
        Seq(2, 1, 1, 4, 2),
      // 0.9555555582046509
        Seq(1, 1, 1, 1, 4, 2),
      // 0.9666666388511658
        Seq(1, 1, 1, 1, 1, 1, 2, 2),
      // 0.9809523820877075
        Seq(1, 1, 1, 1, 1, 1, 2, 1, 1)
    )
    expectedThresholds zip expectedCutSizes foreach { case (expectedThreshold, expectedCutSizes) =>
      val cuts = dendrogram cutAt expectedThreshold
      assert(cuts.map(_.clusters.size) == expectedCutSizes)
    }
  }

  test("Dumps dendrogram to dot file") {
    val dotContent = Dendrogram.toDot(dendrogram, names, "Test Dendrogram")

    val tempFile = File.createTempFile("dendrogram_", ".dot")

    val out = new PrintWriter(new FileWriter(tempFile), true)
    out.println(dotContent)
    out.close()

    val process = Runtime.getRuntime.exec(s"dot -O -Tpng ${tempFile.getAbsolutePath}")
    val exitValue = process.waitFor()
    assert(exitValue == 0)
  }
}
