import java.io.{File, PrintWriter}
import java.nio.file.{Files, Paths}

import scala.collection.mutable.{ArrayBuffer, HashMap}

object ODAGCalculator {

  def main(args: Array[String]): Unit = {
    var odagsCount = 0
    var totalOdagsCapacities:Long = 0
    var actualStoredEmbeddings:Long = 0
    var totalUtilization:Double = 0
    var averageUtilization:Double = 0

    var odags: ArrayBuffer[ODAG] = ArrayBuffer()

    val odagsPath = args(0)
    val embeddingSize = args(1).toInt

    removeEmptyFiles(odagsPath)
    val files = (new File(odagsPath)).listFiles()
    files.foreach(file => {
      val odag = getODAG(file.getAbsolutePath, embeddingSize)
      if (odag != null)
        odags += odag
    })

    val freqFile = "/home/ehussein/Downloads/ArabesqueTesting/fsm/Results/CS_Connections_Gap_Frequencies_Arabesque.csv"
    val divisionCostFile = "/home/ehussein/Downloads/ArabesqueTesting/fsm/Results/YT_Frequencies_Arabesque.csv"

    //calculateODAGStats(odags)
    //calculateConnectionsFrequencies(odags,freqFile)
    calculateDivisionCost(odags, divisionCostFile, embeddingSize)
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

  def getArrayOfInts(neighbStr: String): ArrayBuffer[Int] = {
    var neighbours = new ArrayBuffer[Int]()

    // if there are no neighbours
    if (neighbStr.length <= 2)
      return new ArrayBuffer[Int]()

    neighbours = (neighbStr.substring(1, neighbStr.length - 1)).split(", ").map(x => x.toInt).to[ArrayBuffer]

    neighbours
  }

  def getODAG(odagFileName: String, numOfDomains: Int): ODAG = {
    val odag = new ODAG()
    var domainSize = new ArrayBuffer[Int]()
    val source = scala.io.Source.fromFile(odagFileName)
    val lines = try source.getLines.to[Array] finally source.close()
    var lineNumber = 0
    val odagCapacityLineNumber = 1
    val numOfEmbeddingsLineNumber = 2
    val numOfDomainsLineNumber = 3

    // check if this is the odag with matched number of domains
    if (lines(numOfDomainsLineNumber).split("=").last.toInt != numOfDomains)
      return null

    lineNumber += 4

    odag.numOfDomains = lines(numOfDomainsLineNumber).split("=").last.toInt
    odag.setCapacity(lines(odagCapacityLineNumber).split("=").last.toLong)
    odag.setNumActualEmbeddings(lines(numOfEmbeddingsLineNumber).split("=").last.toLong)

    odag.domains = new ArrayBuffer[HashMap[Int, ArrayBuffer[Int]]](odag.numOfDomains)

    var i = 0
    while (i < odag.numOfDomains) {
      domainSize += lines(lineNumber).split("=").last.toInt
      odag.domains += new HashMap[Int, ArrayBuffer[Int]]()
      i += 1
      lineNumber += 1
    }

    i = 0
    while (i < odag.numOfDomains) {
      var j = 0
      while (j < domainSize(i)) {
        val record = lines(lineNumber).split("=")
        odag.domains(i) += (record(0).toInt ->
          getArrayOfInts(record(1)))
        j += 1
        lineNumber += 1
      }
      i += 1
    }

    odag.getNumTotalEmbeddings
    odag
  }

  def calculateODAGStats(odags: ArrayBuffer[ODAG]) = {
    var odagsCount = 0
    var totalOdagsCapacities:Long = 0
    var actualStoredEmbeddings:Long = 0
    var totalUtilization:Double = 0
    var averageUtilization:Double = 0
    var totalStorage:Long = 0L


    odags.foreach(odag => {
      if(odag.getNumActualEmbeddings != 0) {
        println
        println(s"## ODAG # $odagsCount results ##")
        /**/println("Num Of Domains = " + odag.numOfDomains)
        println("ODAG Storage = " + odag.getStorage())
        /**/println("ODAG Capacity calculated by Arabesque = " + odag.getCapacity)
        println("ODAG Capacity calculated by script = " + odag.getNumTotalEmbeddings)
        println("Num Of Actual Embeddings = " + odag.getNumActualEmbeddings)
        println("Num Of Spurious Embeddings = " + odag.getNumSpuriousEmbeddings)
        val utilizationRatio = (odag.getNumActualEmbeddings.toDouble / odag.getNumTotalEmbeddings) * 100
        totalStorage += odag.getStorage
        totalOdagsCapacities += odag.getNumTotalEmbeddings
        actualStoredEmbeddings += odag.getNumActualEmbeddings
        averageUtilization += utilizationRatio
        print(f"Percentage of utilized space = $utilizationRatio%1.5f %%")
        odagsCount += 1
      }
    })

    totalUtilization = (actualStoredEmbeddings.toDouble / totalOdagsCapacities.toDouble) * 100.0
    averageUtilization = averageUtilization / odagsCount

    println("\n## Summary ##")
    println("Num of ODAGs processed = " + odagsCount)
    println(s"Total Storage = $totalStorage")
    println(s"Total Capacity = $totalOdagsCapacities")
    println(s"Total stored embeddings = $actualStoredEmbeddings")
    println(f"Total Utilization (actual_embeddings_count/Total_Capacity) = $totalUtilization%1.5f %%")
    println(f"Average Utilization (sumOfUtilization/NumOfODAGs) = $averageUtilization%1.5f %%")
  }

  def calculateConnectionsFrequencies(odags: ArrayBuffer[ODAG], fileName: String) = {
    var freqs = new HashMap[Int, Int]()
    var odagsCount = 0
    val writer: PrintWriter = new PrintWriter(fileName)

    var count = 0

    // sort the connections
    odags.foreach(o => {
      count = 0
      o.domains.foreach(domain => {
        domain.keys.foreach(key => {
          /*
          println(key)
          println(domain.contains(key))
          println(domain(key))
          println(s"Domain # $count")
          println
          */

          domain(key) = domain(key).sorted
        })

        count += 1
      })
    })

    var i = 0
    var diff = 0

    odags.foreach(o => {
      o.domains.foreach(domain => {
        domain.values.foreach(connections => {
          i = 0
          if (connections.length > 1) {
            while (i < connections.length - 1) {
              diff = connections(i + 1) - connections(i)
              if(freqs.contains(diff))
                freqs(diff) += 1
              else
                freqs.put(diff,1)

              i += 1
            }
          }
        })
      })
    })

    // sort by frequency descending
    //var freqsSortedByFrequency = freqs.toArray.sortWith((pair1,pair2) => pair1._2 > pair2._2 )
    var freqsSortedByGap = freqs.toArray.sortWith((pair1,pair2) => pair1._1 < pair2._1 )

    writer.println("Gap,Frequency")

    freqsSortedByGap.foreach(f => {
      writer.println(s"${f._1},${f._2}")
    })

    writer.close()
  }

  def calculateDivisionCost(odags: ArrayBuffer[ODAG], fileName: String, embeddingSize: Int) = {
    var vertexFreqs: ArrayBuffer[HashMap[Int, Int]] = new ArrayBuffer[HashMap[Int, Int]](embeddingSize)
    var sizeFreqs = new HashMap[Int, Int]()
    val writer: PrintWriter = new PrintWriter(fileName)
    var i = 0

    while(i < embeddingSize) {
      vertexFreqs += new HashMap[Int, Int]()
      i += 1
    }

    odags.foreach(o => {
      // i is the domain index
      i = 0
      while(i < o.domains.size) {
        val domain = o.domains(i)

        domain.foreach(entry => {
          val vertex = entry._1
          val connections = entry._2

          if (vertexFreqs(i).contains(vertex))
            vertexFreqs(i)(vertex) += 1
          else
            vertexFreqs(i).put(vertex, 1)

          if (sizeFreqs.contains(connections.length))
            sizeFreqs(connections.length) += 1
          else
            sizeFreqs.put(connections.length, 1)
        })

        i += 1
      }
    })

    // count the redundant vertices
    var totalRedundancies = 0
    vertexFreqs.foreach(domainFreqs => {
      domainFreqs.foreach(item => {
        if(item._2 > 1)
          totalRedundancies += item._2
      })
    })

    // sort the size frequency list by the frequency descending
    var sizeFreqsSorted = sizeFreqs.toArray.sortWith((pair1,pair2) => pair1._1 < pair2._1 )

    writer.println(s"Size,Frequency,Redundant vertices,$totalRedundancies")

    sizeFreqsSorted.foreach(f => {
      writer.println(s"${f._1},${f._2}")
    })

    writer.close()
  }
}

