/**
  * Created by ehussein on 6/7/17.
  */
class Embedding {
  var content : Array[Int] = _

  def buildFromString(str: String): Embedding = {
    //println(s"EmbStr = $str")
    content = str.split(" ").map(s => s.toInt)
    this
  }
}
