import scala.io.Source

object D13T1 {
    def vertical(m : Array[String]) : Int = {
        var v = 0
        for(i <- 0 until m(0).length - 1) {
            var reflect = true
            for(j <- 0 until m.length) {
                if(m(j)(i) != m(j)(i + 1)) {
                    reflect = false
                }
            }
            if (reflect) {
                v = i + 1
            }
        }
        var rev = new Array[String](0)
        for(i <- 0 until m(0).length) {
            rev :+= new String()
            for(j <- 0 until m.length) {
                rev(i) :+= m(j)(i)
            }
        }
        val srt = rev.sorted
        var cnt = 0
        for(i <- 0 until srt.length - 1) {
            if(srt(i) == srt(i+1)) {
                cnt = cnt + 1
            }
        }
        if(cnt >= srt.length/2 - 1 ) {
            return v
        }

        0
    }

    def horizontal(m : Array[String]) : Int = {
        var h = 0
        for(i <- 0 until m.length - 1) {
            if(m(i) == m(i+1)) {
                h = i + 1
            }
        }
        val srt = m.sorted
        var cnt = 0
        for(i <- 0 until srt.length - 1) {
            if(srt(i) == srt(i+1)) {
                cnt = cnt + 1
            }
        }
        if(cnt >= m.length/2 - 1 ) {
            return h
        }
        return 0
    }

    def main(args: Array[String]) = {
        val lines = Source.fromFile("input.txt").getLines.toArray
        var sum = 0
        var m = new Array[String](0)
        for(l <- lines) {
            if(l.size == 0) {
                val v = vertical(m)
                val h = horizontal(m)
                    sum = sum + v
                    sum = sum + 100 * h
                m = new Array[String](0)
            } else {
                m :+= l
            }
        }
        if(m.length != 0) {
            val v = vertical(m)
            val h = horizontal(m)
            sum = sum + v
            sum = sum + 100 * h
        }
        println("Sum = " + sum)
    }
}
// 26933 <- To Low
// 11978 <- to low
