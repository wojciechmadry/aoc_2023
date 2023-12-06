import scala.io.Source
import scala.math.BigInt

object D6T2 {

    def main(args: Array[String]) = {
        val lines = Source.fromFile("input.txt").getLines.toArray
        val time = lines(0).substring(6).split(' ').filter(_.nonEmpty).mkString
        val dist = lines(1).substring(9).split(' ').filter(_.nonEmpty).mkString
        var mult = 1
        val t = BigInt(time)
        val d = BigInt(dist)
        var counter = 0
        for(v <- BigInt(1) until t) {
            val s = v * (t - v)
            if (s > d) {
                counter += 1
            }
        }
        mult = mult * counter
        println("Score = " + mult)
    }
}
