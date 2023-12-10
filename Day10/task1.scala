import scala.io.Source

object D10T1 {
    class Pair(var x: Int, var y: Int)

    def process(p: Pair, lines : Array[String],  map: Array[Array[Int]], dist: Int) : Boolean = {
        val newPos = Array[Pair](Pair(p.x - 1, p.y), Pair(p.x + 1, p.y), Pair(p.x, p.y - 1), Pair(p.x, p.y + 1))
        val d = dist + 1
        val myC = lines(p.y)(p.x)
        for((pos, i) <- newPos.zipWithIndex) {
            if(pos.x >= 0 && pos.y >= 0 && pos.x < lines(0).length && pos.y < lines.length
                && lines(pos.y)(pos.x) != '.') {
                var isCorrect = false
                val c = lines(pos.y)(pos.x)
                if(i == 0 && (myC == 'S' || myC == '-' || myC == 'J' || myC == '7') && c != '|' && c != 'J' && c != '7' && (c == '-' || c == 'L' || c == 'F')) {
                    isCorrect = true
                }
                else if(i == 1 && (myC == 'S' || myC == '-' || myC == 'L' || myC == 'F') && c != '|' && c != 'L' && c != 'F' && (c == '-' || c == 'J' || c == '7')) {
                    isCorrect = true
                }
                else if(i == 2 && (myC == 'S' || myC == '|' || myC == 'L' || myC == 'J')&& c != 'L' && c != 'J' && c != '-' && (c == '|' || c == 'F' || c == '7')) {
                    isCorrect = true
                }
                else if(i == 3 && (myC == 'S' || myC == '|' || myC == 'F' || myC == '7') && c != '7' && c != 'F' && c != '-' && (c == '|' || c == 'L' || c == 'J')) {
                    isCorrect = true
                }
                if (isCorrect && (map(pos.y)(pos.x) == 0 || map(pos.y)(pos.x) > d)) {
                    map(pos.y)(pos.x) = d
                    process(pos, lines, map, d)
                }
            }
        }
        return true
    }

    def printMap(map: Array[Array[Int]]) = {
        for(row <- map) {
            for(col <- row) {
                print(col + " ")
            }
            println("")
        }
    }
    def calculateDist(filename: String): Int = {
        val lines = Source.fromFile(filename).getLines.toArray
        var map = new Array[Array[Int]](lines.length)
        var start = Pair(0, 0)
        for(i <- 0 until lines.length) {
            map(i) = new Array[Int](lines(i).length)
            for(j <- 0 until lines(i).length) {
                map(i)(j) = 0
                if(lines(i)(j) == 'S') {
                    start.x = j
                    start.y = i
                }
            }
        }
        // println("Start position: x = " + start.x + ", y = " + start.y)
        process(start, lines, map, 0)
        // printMap(map)
        var maxDist = -1
        for(row <- map) {
            val maxInRow = row.max
            if (maxInRow > maxDist) {
                maxDist = maxInRow
            }
        }
        //println("Max distance = " + maxDist)
        return maxDist
    }
    def main(args: Array[String]) = {
        assert(calculateDist("example.txt") == 4, "example.txt != 4")
        assert(calculateDist("example2.txt") == 4, "example2.txt != 4")
        assert(calculateDist("example3.txt") == 8, "example3.txt != 8")
        assert(calculateDist("example4.txt") == 8, "example4.txt != 8")
        println("Dist = " + calculateDist("input.txt"))
    }
}
