import scala.io.Source
import scala.util.Try

object D1T2 {
    val NUMBERS = Array[String]("one", "two", "three", "four", "five", "six", "seven", "eight", "nine")
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
            var line: String = ""
            for((ch, i) <- l.view.zipWithIndex) {
                if (ch >= '0' && ch <= '9') line += ch
                else {
                    for((n, ni) <- NUMBERS.view.zipWithIndex) {
                        if(i + n.size <= l.size) {
                            var num_match : Boolean = true
                            var tem_i : Int = i
                            for(ch_n <- n) {
                                if(l(tem_i) != ch_n) {
                                    num_match = false
                                }
                                tem_i += 1
                            }
                            if (num_match) {
                                line += (ni + 1 + '0').toChar
                            }
                        }
                    }
                }
            }
            sum = sum + findFirst(line) + findLast(line)
        }
        println("Sum = "+ sum)
    }
}
