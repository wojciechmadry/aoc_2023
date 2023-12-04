import scala.io.Source

object D4T1 {
    def main(args: Array[String]) = {
        val lines = Source.fromFile("input.txt").getLines.toArray
        var sum : Int = 0
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
            if (cnt > 0) {
                sum = sum + scala.math.pow(2, cnt - 1).intValue()
            }
        }
        println("Sum = " + sum)
    }
}
