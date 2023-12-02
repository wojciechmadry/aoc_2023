import scala.io.Source

object D2T1 {

    def main(args: Array[String]) = {
        val lines = Source.fromFile("input.txt").getLines.toArray
        var redInBag : Int = 12
        var greenInBag : Int = 13
        var blueInBag : Int = 14
        var gameIdx : Int = 1
        var sumIdxOfGames : Int = 0

        for (line <- lines) {
            var l = line + ';'
            var idx : Int = l.indexOf(':') + 1
            var green : Int = 0
            var blue : Int = 0
            var red : Int = 0
            while(idx < l.length()) {
                val idxSemi : Int = l.indexOf(';', idx)
                val ss = l.substring(idx, idxSemi)
                val splitted = ss.split(',')
                for(spl <- splitted) {
                    val s = spl.trim()
                    val num_color = s.split(' ')
                    if (num_color(1) == "red") {
                        red = red.max(num_color(0).toInt)
                    } else if (num_color(1) == "blue") {
                        blue = blue.max(num_color(0).toInt)
                    } else {
                        green = green.max(num_color(0).toInt)
                    }
                }
                idx = idxSemi + 1
            }
            if (redInBag >= red && blueInBag >= blue && greenInBag >= green) {
                sumIdxOfGames = sumIdxOfGames + gameIdx
            }
            gameIdx += 1
        }
        println("Idx sum = " + sumIdxOfGames)
    }
}
