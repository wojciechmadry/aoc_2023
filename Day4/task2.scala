import scala.io.Source

object D4T2 {
    def main(args: Array[String]) = {
        val lines = Source.fromFile("input.txt").getLines.toArray
        var multiplier = Array.fill(lines.length)(1)
        for ((line, i) <- lines.zipWithIndex) {
            val idx = line.indexOf(':') + 2
            val bothSide = line.substring(idx, line.length()).split('|')
            val leftSide = bothSide(0).split(' ').filter(_.nonEmpty)
            val rightSide = bothSide(1).split(' ').filter(_.nonEmpty)
            var cnt = 0
            for(myNumber <- leftSide) {
                if (rightSide.contains(myNumber)) {
                    cnt = cnt + 1
                }
            }
            for( j <- i + 1 to i + cnt) {
                multiplier(j) = multiplier(j) + multiplier(i)
            }
        }
        println("Sum = " + multiplier.sum)
    }
}
