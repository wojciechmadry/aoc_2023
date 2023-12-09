import scala.io.Source

object D9T2 {
    def find_seq(values : Array[Int]) : Array[Int] = {
        var seq = new Array[Int](values.length - 1)
        for(idx <- 0 until values.length - 1) {
            seq(idx) = values(idx + 1) - values(idx)
        }
        return seq
    }

    def main(args: Array[String]) = {
        val lines = Source.fromFile("input.txt").getLines.toArray
        var sum_history = 0
        for(l <- lines) {
            val values = l.split(' ').filter(_.nonEmpty).map(_.trim()).map(_.toInt)
            var sequences = new Array[Array[Int]](0)
            sequences :+= values
            while(!sequences.last.forall(_ == 0)) {
                val seq = find_seq(sequences.last)
                sequences :+= seq
            }
            var history = 0
            for(idx <- sequences.length - 2 to 0 by -1) {
                history = sequences(idx).head - history
            }
            sum_history = sum_history + history
        }

        println("Sum = " + sum_history)
    }
}
