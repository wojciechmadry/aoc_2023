import scala.io.Source

object D3T1 {
    type Point = (Int,Int)
    def isMatch(text: Array[String], i : Int, j : Int) : Boolean = {
        val idxArr = Array[Point]((i - 1, j - 1), (i - 1, j), (i - 1, j + 1), (i, j - 1), (i, j + 1), (i + 1, j - 1), (i + 1, j), (i + 1, j + 1))
        var found = false
        for (idx <- idxArr) {
            if(idx._1 >= 0 && idx._1 < text.length
                && idx._2 >= 0 && idx._2 < text(idx._1).length
                && text(idx._1)(idx._2) != '.'
                && ( text(idx._1)(idx._2) < '0'
                || text(idx._1)(idx._2) > '9')) {
                found = true
            }
        }
        found
    }

    def main(args: Array[String]) = {
        val lines = Source.fromFile("input.txt").getLines.toArray
        var sum : Int = 0
        var str : String = ""
        for ((line, i) <- lines.zipWithIndex) {
            var matched: Boolean = false
            for ((ch, j) <- line.zipWithIndex) {
                if(ch >= '0' && ch <= '9') {
                    str = str + ch
                    if (isMatch(lines, i, j)) {
                        matched = true
                    }
                }
                else {
                    if (matched) {
                        sum = sum + str.toInt
                    }
                    str = ""
                    matched = false
                }
            }
            if (matched) {
                sum = sum + str.toInt
            }
            str = ""
        }
        println("Part sum = " + sum)
    }
}
