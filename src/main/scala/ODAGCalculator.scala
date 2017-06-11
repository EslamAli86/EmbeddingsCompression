import java.io.{File}
import java.nio.file.{Files, Paths}
import scala.collection.mutable.{ArrayBuffer, HashMap}

object ODAGCalculator {

  def main(args: Array[String]): Unit = {
    var odagsCount = 0
    var totalOdagsCapacities:Long = 0
    var actualStoredEmbeddings:Long = 0
    var totalUtilization:Double = 0
    var averageUtilization:Double = 0

    removeEmptyFiles(args(0))
    val files = (new File(args(0))).listFiles()
    files.foreach(file => {
      val odag = getODAG(file.getAbsolutePath, args(1).toInt)

      if (odag != null) {
        println
        println("Processiwng file: " + file.getName)
        println(s"================================= ${file.getName} results ==================================")
        println("Num Of Domains = " + odag.numOfDomains)
        println("ODAG Capacity calculated by Arabesque = " + odag.getOdagCapacity)
        println("ODAG Capacity calculated by script = " + odag.getNumTotalEmbeddings)
        println("Num Of Actual Embeddings = " + odag.getNumActualEmbeddings)
        println("Num Of Spurious Embeddings = " + odag.getNumSpuriousEmbeddings)
        val utilizationRatio = (odag.getNumActualEmbeddings.toDouble / odag.getNumTotalEmbeddings) * 100
        totalOdagsCapacities += odag.getNumTotalEmbeddings
        actualStoredEmbeddings += odag.getNumActualEmbeddings
        averageUtilization += utilizationRatio
        println(f"Percentage of utilized space = $utilizationRatio%1.5f %%")
        odagsCount += 1
      }
    })

    totalUtilization = (actualStoredEmbeddings.toDouble / totalOdagsCapacities.toDouble) * 100.0
    averageUtilization = averageUtilization / odagsCount

    println("=======================================================================================")
    println("Num of ODAGs processed = " + odagsCount)
    println(s"Total Capacity = $totalOdagsCapacities")
    println(s"Total stored embeddings = $actualStoredEmbeddings")
    println(f"Total Utilization (actual_embeddings_count/Total_Capacity) = $totalUtilization%1.5f %%")
    println(f"Average Utilization (sumOfUtilization/NumOfODAGs) = $averageUtilization%1.5f %%")
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
      return null

    neighbours = (neighbStr.substring(1, neighbStr.length - 1)).split(", ").map(x => x.toInt).to[ArrayBuffer]

    /*    println
        x.foreach(n => print(n + "\t"))
        println*/

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

    //numOfDomains = lines(numOfDomainsLineNumber).split("=").last.toInt
    odag.numOfDomains = lines(numOfDomainsLineNumber).split("=").last.toInt
    odag.setOdagCapacity(lines(odagCapacityLineNumber).split("=").last.toLong)
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
        //println(lines(lineNumber))
        val record = lines(lineNumber).split("=")
        //record.foreach(println)
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
}

