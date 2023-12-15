import scala.io.Source

object D15T1 {
    def hash(s : String) : Int = {
        var res = 0
        for(c <- s) {
            res = res + c.toInt
            res = res * 17
            res = res % 256
       }
        res
    }
    def main(args: Array[String]) = {
        val lines = Source.fromFile("input.txt").getLines.toArray
        val values = lines(0).split(',').filter(_.nonEmpty)
        var sum = 0
        for(v <- values) {
            sum = sum + hash(v)
        }
        println("Hash sum = " + sum)
    }
}
