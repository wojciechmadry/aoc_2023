import scala.io.Source

object D6T1 {

    def main(args: Array[String]) = {
        val lines = Source.fromFile("input.txt").getLines.toArray
        val time = lines(0).substring(6).split(' ').filter(_.nonEmpty).map(_.toInt)
        val dist = lines(1).substring(9).split(' ').filter(_.nonEmpty).map(_.toInt)
        var mult = 1
        for(i <- 0 until time.length) {
            val t = time(i)
            val d = dist(i)
            var counter = 0
            for(v <- 1 until t) {
                val s = v * (t - v)
                if (s > d) {
                    counter += 1
                }
            }
            mult = mult * counter
        }
        println("Score = " + mult)
    }
}
