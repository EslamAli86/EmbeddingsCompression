import java.io.File
import java.nio.file.{Files, Paths}
import scala.collection.mutable.{ArrayBuffer}

/**
  * Created by ehussein on 6/7/17.
  */
object KMeansODAGs {
  // k is the number of clusters/odags
  var k = 1
  def main(args: Array[String]): Unit = {
    if(args.length < 2) {
      println("Illegal number of arguments")
      return
    }

    k = args(1).toInt
    val odags: ArrayBuffer[ODAG] = ArrayBuffer.fill(k)(new ODAG) //new ArrayBuffer[ODAG](k)

    removeEmptyFiles(args(0))
    val files = (new File(args(0))).listFiles()
    files.foreach(file => {
      val source = scala.io.Source.fromFile(file.getAbsolutePath)
      val lines = try source.getLines.to[Array] finally source.close()
      // parse and convert the read lines into embeddings
      val embeddings = lines.map(line => {
        (new Embedding).buildFromString(line)
      })

      embeddings.foreach(embedding => {
        var odagsCounter = 0
        var odagIndex = 0
        var minAdditionCost = Int.MaxValue
        var embeddingCost = 0

        // iterate over all k odags to find the most
        // suitable one (which has the minimum cost)
        while(odagsCounter < odags.size) {
          embeddingCost = odags(odagsCounter).getEmbeddingCost(embedding)
          if(embeddingCost < minAdditionCost) {
            odagIndex = odagsCounter
            minAdditionCost = embeddingCost
          }
          odagsCounter += 1
        }

        // add the embedding to the odag with the lowest addition cost
        odags(odagIndex).add(embedding)
      })
    })
  }

  def removeEmptyFiles(dir: String) = {
    val dirPath = Paths.get(dir)
    if (Files.exists(dirPath)) {
      (new File(dir)).listFiles().foreach(file => {
        if (file.length() == 0 || file.isHidden)
          file.delete()
      })
    }
  }

}
