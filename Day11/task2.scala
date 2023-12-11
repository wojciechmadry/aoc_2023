import scala.io.Source

object D11T2 {
    class Pair(var x: Int, var y: Int)
    class PairB(var x: BigInt, var y: BigInt)

    def main(args: Array[String]) = {
        val linesNoExpand = Source.fromFile("input.txt").getLines.toArray
        //val lines = linesNoExpand
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
        val lines = linesNoExpand
        var pairs = new Array[Pair](0)
        for((l, y) <- lines.zipWithIndex) {
            for((c, x) <- l.zipWithIndex) {
                if(c == '#') {
                    pairs :+= Pair(x, y);
                }
            }
        }
        val MILION = 1000000 - 1 // Minues one, because there is already one space existing in the map
        var sum = BigInt(0)
        for((p, i) <- pairs.zipWithIndex) {
            for(j <- i + 1 until pairs.length) {
                val p2 = pairs(j)
                var pxa = 0
                var p2xa = 0
                for(col <- missingColls) {
                    if(p.x < col) {
                        pxa = pxa + 1
                    }
                    if(p2.x < col) {
                        p2xa = p2xa + 1
                    }
                }

                var pya = 0
                var p2ya = 0
                for(row <- missingRows) {
                    if(p.y < row) {
                        pya = pya + 1
                    }
                    if(p2.y < row) {
                        p2ya = p2ya + 1
                    }
                }
                pya = pya * MILION
                p2ya = p2ya * MILION
                pxa = pxa * MILION
                p2xa = p2xa * MILION
                val dist = (p.x - p2.x).abs + (p.y - p2.y).abs + (pxa - p2xa).abs + (pya - p2ya).abs
                sum = sum + dist
            }
        }
        println("Sum = " + sum)
    }
}
