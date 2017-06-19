import scala.collection.mutable.{ArrayBuffer, HashMap}

/**
  * Created by ehussein on 6/7/17.
  */
class ODAG {
  var numOfDomains = 0
  var domains: ArrayBuffer[HashMap[Int, ArrayBuffer[Int]]] = _
  // total = actual + spurious
  protected var storage = 0L
  protected var capacity = 0L
  protected var numTotalEmbeddings = 0L
  // num of actual embeddings is stored in the odag file
  protected var numActualEmbeddings = 0L
  protected var numSpuriousEmbeddings = 0L
  //var domains = new HashMap[Int, HashMap[_,_]]
  var enumeratios: ArrayBuffer[HashMap[Int, Long]] = _

  protected def calcNumTotalEmbeddings(): Long = {
    var total = 0L

    // if it is empty then it is empty :D
    if(domains(0).size == 0)
      0L

    enumeratios = new ArrayBuffer[HashMap[Int, Long]]()
    domains.foreach(_ => enumeratios += new HashMap[Int, Long]())

    // init the last domain with 1
    var i = domains.length - 1
    var j = 0
    domains(i).foreach(entry => {
      enumeratios(i)(entry._1) = 1L
    })

    i = domains.length - 2

    while(i >= 0) {
      var domain = domains(i)
      domain.foreach(entry => {
        val key = entry._1
        val neighbours = entry._2
        var totalNeighbours = 0L
        //enumeratios(i + 1).keys.toArray.sorted.foreach(k => print(k + " "))
        neighbours.foreach(neighb => {
          //println(s"Domain.num = ${i+1}, NeighbID = $neighb")
          totalNeighbours += enumeratios(i + 1)(neighb)
        })
        enumeratios(i)(key) = totalNeighbours
      })
      i -= 1
    }
    enumeratios(0).foreach(entry => total += entry._2)
    total
  }

  def getNumTotalEmbeddings: Long = {
    if(numTotalEmbeddings == 0) {
      numTotalEmbeddings = calcNumTotalEmbeddings
    }
    numTotalEmbeddings
  }

  def getNumSpuriousEmbeddings: Long = {
    getNumTotalEmbeddings - numActualEmbeddings
    //capacity - numActualEmbeddings
  }

  def getNumActualEmbeddings: Long = { numActualEmbeddings }

  def setNumActualEmbeddings(num: Long) = { numActualEmbeddings = num }

  def getStorage(): Long = { storage }

  def setStorage(storage: Long) = { this.storage = storage }

  def increaseStorage(amount: Int) = { this.storage += amount}

  def getCapacity: Long = {
    if(capacity == 0)
      capacity = getNumTotalEmbeddings
    capacity
  }

  def setCapacity(num: Long) = { capacity = num }

  // calculates the cost of adding an embedding to the odag
  // i.e. how many variables needed to add the new embedding
  def getEmbeddingCost(e:Embedding) : Int = {
    if(e.content.length != domains.size) {
      e.content.length
    }

    var cost: Int = 0
    var i: Int = 0
    val emb = e.content

    while(i < domains.size) {
      val elem = emb(i)
      // in case current element is NOT in the corresponding domain in the odag
      // then add the cost of adding the element and its connection
      if( !domains(i).contains( elem ) ) {
        cost += 2
      }
        // in case current element is in the corresponding domain in the odag
        // we need to add a connection to the next one
      else if(i < domains.size - 1) // so NO ArraysOutOfIndex exceptions
      {
        // if no connection exist
        if( !domains(i)(elem).contains( emb(i + 1) ) )
          cost += 1
      }
      i += 1
    }

    cost
  }

  def add(e:Embedding) = {
    var i: Int = 0
    val emb = e.content

    while(i < domains.size) {
      var elem = emb(i)
      // in case current element is NOT in the corresponding domain in the odag
      // then add the element
      if( !domains(i).contains( elem ) ) {
        domains(i) += (elem -> new ArrayBuffer[Int]())
      }
      // if no connection exist add a connection to the next element
      var domainEntry = domains(i)(elem)
      if( i < (domains.size - 1) && !domainEntry.contains( emb(i + 1) ) ) {
        domains(i)(elem) += emb(i + 1)
      }
      i += 1
    }
    numActualEmbeddings += 1
  }
}

