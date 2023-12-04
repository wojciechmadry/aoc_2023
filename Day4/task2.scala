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
            var idxMult = i + 1
            for(myNumber <- leftSide) {
                if (rightSide.contains(myNumber)) {
                    multiplier(idxMult) = multiplier(idxMult) + multiplier(i)
                    idxMult += 1
                }
            }
        }
        println("Sum = " + multiplier.sum)
    }
}
