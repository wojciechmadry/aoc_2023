import scala.io.Source

object D15T2 {
    class Pair(var x: Int, var y: Int)

    def hash(s : String) : Int = {
        var res = 0
        for(c <- s) {
            res = res + c.toInt
            res = res * 17
            res = res % 256
       }
        res
    }
    def main(args: Array[String]) = {
        val lines = Source.fromFile("input.txt").getLines.toArray
        val values = lines(0).split(',').filter(_.nonEmpty)
        var map = new Array[collection.mutable.SortedMap[String, Pair]](256)
        for(i <- 0 until map.length) {
            map(i) = collection.mutable.SortedMap[String, Pair]()
        }
        for(v <- values) {
            var sign : Char = '-'
            val idxMinus = v.indexOf('-')
            val idxEq = v.indexOf('=')
            if(idxEq != -1) {
                sign = '='
            }
            val spl = v.split(sign)
            val label = spl(0)
            val box = hash(label)
            if (sign == '-') {
                if(map(box).contains(label)) {
                    val oldVal = map(box)(label).y
                    map(box).remove(label)
                    for((kk, vv) <- map(box)) {
                        if(map(box)(kk).y > oldVal) {
                            val newV = Pair(vv.x, vv.y - 1)
                            map(box)(kk) = newV
                        }
                    }
                }
            }
            else if(map(box).contains(label)) {
                map(box)(label).x = spl(1).toInt
            }
            else {
                val value = spl(1).toInt
                var max = 0
                for((kk, vv) <- map(box)) {
                    if (vv.y > max) {
                        max = vv.y
                    }
                }
                map(box)(label) = Pair(value, max + 1)
            }
        }
        var focalSum = 0
        for((box, i) <- map.zipWithIndex) {
            for((v, j) <- box.zipWithIndex) {
                val focal = (i + 1) * v._2.y * v._2.x
                focalSum = focalSum + focal
            }
        }
        println("Focal sum = " + focalSum)
    }
}
