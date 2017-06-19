import java.io.File
import java.nio.file.{Files, Paths}

import scala.collection.mutable.{ArrayBuffer, HashMap}

/**
  * Created by ehussein on 6/7/17.
  */
object KMeansODAGs {
  // maxK is the number of clusters/odags
  var maxK = 1
  // the amount to increment between 1 and maxK
  var incrementalAmount = 1
  var embeddingSize = 0
  var filesPath:String = _

  def main(args: Array[String]): Unit = {
    if(args.length < 3) {//4) {
      println("Wrong number of arguments")
      return
    }

    filesPath = args(0)
    maxK = args(1).toInt
    embeddingSize = args(2).toInt

    removeEmptyFiles(filesPath)
    println(s"DirectoryPath = $filesPath")
    println(s"NumOfODAGs = $maxK")
    //println(s"incr = ${args(2)}")
    println(s"EmbeddingSize = $embeddingSize")


    var odags: ArrayBuffer[ODAG] = ArrayBuffer.fill(maxK)(new ODAG) //new ArrayBuffer[ODAG](k)
    odags = odags.map( odag => {
      odag.numOfDomains = embeddingSize
      odag.domains = new ArrayBuffer[HashMap[Int, ArrayBuffer[Int]]](odag.numOfDomains)

      var i = 0
      while (i < odag.numOfDomains) {
        odag.domains += new HashMap[Int, ArrayBuffer[Int]]()
        i += 1
      }
      odag
    })

    var embeddingsCounter = 0

    val files = (new File(filesPath)).listFiles()
    files.foreach(file => {
      val source = scala.io.Source.fromFile(file.getAbsolutePath)
      val lines = try source.getLines.to[Array] finally source.close()
      // parse and convert the read lines into embeddings
      val embeddings = lines.map(line => {
        (new Embedding).buildFromString(line)
      })

      embeddings.foreach(embedding => {
        var odagsCounter = 0
        var odagIndices = new ArrayBuffer[Int]()
        var minAdditionCost = Int.MaxValue
        var embeddingCost = 0

        embeddingsCounter += 1

        // iterate over all k odags to find the most suitable one (which has the minimum cost)
        while(odagsCounter < odags.size) {
          embeddingCost = odags(odagsCounter).getEmbeddingCost(embedding)

          //println(s"The cost of adding embedding #$embeddingsCounter in odag#$odagsCounter = $embeddingCost")

          if(embeddingCost < minAdditionCost) {
            odagIndices.clear()
            odagIndices += odagsCounter
            minAdditionCost = embeddingCost
          }
          else if(embeddingCost == minAdditionCost) {
            odagIndices += odagsCounter
          }
          odagsCounter += 1
        }

        if(odagIndices.length != 1) {
          // we will select the smallest odag, otherwise we will select randomly
          odagIndices = odagIndices.sortWith((x,y) => odags(x).getStorage <= odags(y).getStorage)
        }

        odags(odagIndices(0)).add(embedding)
        odags(odagIndices(0)).increaseStorage(minAdditionCost)
      })
    })

    //var freqFile = "/home/ehussein/Downloads/ArabesqueTesting/fsm/Results/CS_Connections_Gap_Frequencies.csv"
    val divisionCostFile = "/home/ehussein/Downloads/ArabesqueTesting/fsm/Results/CS_Frequencies_OneODAG.csv"

    //ODAGCalculator.calculateODAGStats(odags)
    //ODAGCalculator.calculateConnectionsFrequencies(odags, freqFile)
    ODAGCalculator.calculateDivisionCost(odags, divisionCostFile, embeddingSize)
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
