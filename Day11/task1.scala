import scala.io.Source

object D11T1 {
    class Pair(var x: Int, var y: Int)

    def main(args: Array[String]) = {
        val linesNoExpand = Source.fromFile("input.txt").getLines.toArray
        var missingColls = new Array[Int](0)
        for(i <- 0 until linesNoExpand(0).length) {
            var found = false
            for(j <- 0 until linesNoExpand.length) {
                if(linesNoExpand(j)(i) == '#') {
                    found = true
                }
            }
            if (found == false) {
                missingColls :+= i
            }
        }
        var missingRows = new Array[Int](0)
        for(i <- 0 until linesNoExpand.length) {
            var found = false
            for(j <- 0 until linesNoExpand(i).length) {
                if(linesNoExpand(i)(j) == '#') {
                    found = true
                }
            }
            if(found == false) {
                missingRows :+= i
            }
        }

        val newRowSize = linesNoExpand.length + missingRows.length
        val newCollSize = linesNoExpand(0).length + missingColls.length
        val lines = new Array[Array[Char]](newRowSize)
        for(i <- 0 until newRowSize) {
            lines(i) = new Array[Char](newCollSize)
            for(j <- 0 until newCollSize) {
                lines(i)(j) = '.'
            }
        }

        for(i <- 0 until linesNoExpand.length) {
            for(j <- 0 until linesNoExpand(i).length) {
                if(linesNoExpand(i)(j) == '#') {
                    var addRows = 0
                    for(r <- missingRows) {
                        if(i > r) {
                            addRows = addRows + 1
                        }
                    }
                    var addColls = 0
                    for(c <- missingColls) {
                        if(j > c) {
                            addColls = addColls + 1
                        }
                    }
                    lines(i + addRows)(j + addColls) = '#'
                }
            }
        }

        var pairs = new Array[Pair](0)
        for((l, y) <- lines.zipWithIndex) {
            for((c, x) <- l.zipWithIndex) {
                if(c == '#') {
                    pairs :+= Pair(x, y);
                }
            }
        }
        var sum = 0
        for((p, i) <- pairs.zipWithIndex) {
            for(j <- i + 1 until pairs.length) {
                val dist = (p.x - pairs(j).x).abs + (p.y - pairs(j).y).abs
                sum = sum + dist
            }
        }
        println("Sum = " + sum)
    }
}
