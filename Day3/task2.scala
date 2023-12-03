import scala.io.Source

object D3T2 {
    type Point = (Int,Int)
    def Match(text: Array[String], i : Int, j : Int) : Array[Point] = {
        val idxArr = Array[Point]((i - 1, j - 1), (i - 1, j), (i - 1, j + 1), (i, j - 1), (i, j + 1), (i + 1, j - 1), (i + 1, j), (i + 1, j + 1))
        var founds : Array[Point] = new Array[Point](0)
        for (idx <- idxArr) {
            if(idx._1 >= 0 && idx._1 < text.length
                && idx._2 >= 0 && idx._2 < text(idx._1).length
                && text(idx._1)(idx._2) == '*') {
                    founds :+= idx
                }
        }
        return founds
    }

    def main(args: Array[String]) = {
        val lines = Source.fromFile("input.txt").getLines.toArray
        var str : String = ""
        var stars = collection.mutable.Map[Point, Array[Int]]()
        for ((line, i) <- lines.zipWithIndex) {
            var idxToAdd = new Array[Point](0)
            for ((ch, j) <- line.zipWithIndex) {
                if(ch >= '0' && ch <= '9') {
                    str = str + ch
                    val m = Match(lines, i, j)
                    if (m.length != 0) {
                        for(idx <- m) {
                            if(!idxToAdd.contains(idx)) {
                                idxToAdd :+= idx
                            }
                        }
                    }
                }
                else if (!str.isEmpty()){
                    val strAsInt = str.toInt
                    for (idx <- idxToAdd) {
                        if(!stars.contains(idx)) {
                            stars(idx) = new Array[Int](0)
                        }
                        stars(idx) :+= strAsInt
                    }
                    str = ""
                    idxToAdd = new Array[Point](0)
                } else {
                    str = ""
                    idxToAdd = new Array[Point](0)
                }
            }
            if(!str.isEmpty()) {
                val strAsInt = str.toInt
                for (idx <- idxToAdd) {
                    if(!stars.contains(idx)) {
                        stars(idx) = new Array[Int](0)
                    }
                    stars(idx) :+= strAsInt
                }
            }
            idxToAdd = new Array[Point](0)
            str = ""
        }
        var gearRatio : Int = 0
        for ((k, v) <- stars) {
            if(v.length == 2) {
                gearRatio = gearRatio + v(0) * v(1)
            }
        }
        println("Gear ratio = " + gearRatio)
    }
}
