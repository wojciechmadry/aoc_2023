import scala.io.Source

object D13T1 {
    def vertical(m : Array[String]) : Int = {
        var v = new Array[Int](0)
        for(i <- 0 until m(0).length - 1) {
            var reflect = true
            for(j <- 0 until m.length) {
                if(m(j)(i) != m(j)(i + 1)) {
                    reflect = false
                }
            }
            if (reflect) {
                v :+= i + 1
            }
        }
        if(v.length == 0) {
            return 0
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
        if(cnt >= 1) {
            return v.minBy(va => math.abs(va - srt.length/2 - 1))
        }
        0
    }

    def horizontal(m : Array[String]) : Int = {
        var h = new Array[Int](0)
        for(i <- 0 until m.length - 1) {
            if(m(i) == m(i+1)) {
                h :+= i + 1
            }
        }
        if(h.length == 0) {
            return 0
        }
        val srt = m.sorted
        var cnt = 0
        for(i <- 0 until srt.length - 1) {
            if(srt(i) == srt(i+1)) {
                cnt = cnt + 1
            }
        }
        if(cnt >= 1) {
            return h.minBy(va => math.abs(va - m.length/2 - 1))
        }
        return 0
    }

    def main(args: Array[String]) = {
        val lines = Source.fromFile("example.txt").getLines.toArray
        var sum = 0
        var m = new Array[String](0)
        for(l <- lines) {
            if(l.size == 0) {
                val v = vertical(m)
                val h = horizontal(m)
                sum = sum + v
                sum = sum + 100 * h
                println(m.length + " = " + (v + 100 * h))
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
// 32500 <- Not correct
