import scala.io.Source
import scala.util.Try

object D1T1 {
    def findFirst( s:String ) : Int = {
        val c : Char = s.find(c => c >= '0' && c <= '9').getOrElse(0)
        (c - '0').toInt * 10
    }
    def findLast( s:String ) : Int = {
        val c : Char = s.findLast(c => c >= '0' && c <= '9').getOrElse(0)
        (c - '0').toInt
    }

    def main(args: Array[String]) = {
        val lines = Source.fromFile("input.txt").getLines.toArray
        var sum : Int = 0
        for (l <- lines) {
            sum = sum + findFirst(l) + findLast(l)
        }
        println("Sum = "+ sum)
    }
}
