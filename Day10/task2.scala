import scala.io.Source

object D10T2 {
    class Pair(var x: Int, var y: Int)

    def fill(p: Pair, map : Array[Array[Char]]) : Boolean = {
        val newPos = Array[Pair](Pair(p.x - 1, p.y), Pair(p.x + 1, p.y), Pair(p.x, p.y - 1), Pair(p.x, p.y + 1))
        for((pos, i) <- newPos.zipWithIndex) {
            if(pos.x >= 0 && pos.y >= 0 && pos.x < map(0).length && pos.y < map.length
                && map(pos.y)(pos.x) == '.') {
                map(pos.y)(pos.x) = 'O'
                fill(pos, map)
            }
        }
        true
    }

    def printMap(map: Array[Array[Char]]) = {
        for(row <- map) {
            for(col <- row) {
                print(col + " ")
            }
            println("")
        }
    }

    def calculateDist(filename: String): Int = {
        val lines = Source.fromFile(filename).getLines.toArray
        var map = new Array[Array[Char]](lines.length)
        for((l, i) <- lines.zipWithIndex) {
            map(i) = new Array[Char](l.length)
            for((c, j) <- l.zipWithIndex) {
                map(i)(j) = c
            }
        }
        for(i <- 0 until map.length) {
            if(map(i)(0) == '.') {
                fill(Pair(0, i), map)
            }
            if(map(i)(map(i).length - 1) == '.') {
                fill(Pair(map(i).length - 1, i), map)
            }
        }
        for(i <- 0 until map(0).length) {
            if(map(0)(i) == '.') {
                fill(Pair(i, 0), map)
            }
            if(map(map.length - 1)(i) == '.') {
                fill(Pair(i, map.length - 1), map)
            }
        }
        printMap(map)
        var enclosed = 0
        for(row <- map) {
            for(c <- row) {
                if(c == '.') {
                    enclosed = enclosed + 1
                }
            }
        }
        return enclosed
    }
    def main(args: Array[String]) = {
        assert(calculateDist("example.txt") == 4, "example.txt != 4")
        assert(calculateDist("example1.txt") == 4, "example1.txt != 4")
        assert(calculateDist("example2.txt") == 8, "example2.txt != 8")
        assert(calculateDist("example3.txt") == 10, "example3.txt != 10")
        //println("Dist = " + calculateDist("input.txt"))
    }
}
