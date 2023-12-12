import scala.io.Source

object D12T2 {
    def cnt(l: Array[Byte]): Array[Int] = {
        var result = new Array[Int](0)
        var count = 0
        for(c <- l) {
            if (c == '.' || c == '?') {
                if(count != 0) {
                    result :+= count
                    count = 0
                }
            }
            else if (c == '#') {
                count = count + 1
            }
        }
        if(count != 0) {
            result :+= count
        }
        result
    }

    def compare(lhs : Array[Byte], org : Array[Int], partial : Boolean, idx : Int) : Boolean = {
        var lhs_cnt = cnt(lhs.slice(0, idx))
        if (partial == false) {
            return lhs_cnt.sameElements(org)
        }
        if(lhs_cnt.size > org.size) {
            return false
        }
        var areSame = true
        for(i <- 0 until lhs_cnt.size - 1) {
            if(lhs_cnt(i) != org(i)) {
                areSame = false
            }
        }
        return areSame
    }

    def genComb(map : Array[Byte], idx : Int, org: Array[Int]) : Int = {
        var combs = 0
        if(idx == map.length) {
            if(compare(map, org, false, idx)) {
                combs = combs + 1
            }
            return combs
        }
        if(!compare(map, org, true, idx)) {
            return 0
        }
        if(map(idx) == '?'){
            map(idx) = '.'
            combs = combs + genComb(map, idx + 1, org)
            map(idx) = '#'
            combs = combs + genComb(map, idx + 1, org)
            map(idx) = '?'
        } else {
            var nextIdx = idx + 1
            while(nextIdx < map.length && map(nextIdx) != '?') {
                nextIdx = nextIdx + 1
            }
            combs = combs + genComb(map, nextIdx, org)
        }
        return combs
    }

    def main(args: Array[String]) = {
        val lines = Source.fromFile("input.txt").getLines.toArray
        var sum = BigInt(0)
        val before = System.currentTimeMillis
        for((l, progress) <- lines.zipWithIndex) {
            val spl = l.split(' ')
            val ar = spl(0)
            val valuesFold = spl(1).split(',').map(_.trim()).filter(_.nonEmpty).map(_.toInt)
            var values = new Array[Int](0)
            val b = ar.getBytes()
            var byteArray = new Array[Byte](0)
            for(i <- 0 until 5) {
                for(c <- b) {
                    byteArray :+= c
                }
                for(v <- valuesFold) {
                    values :+= v
                }
                if(i != 4) {
                    byteArray :+= '?'
                }
            }
            //println(new String(byteArray))
            //for(v <- values) {
            //    print(v + " ")
            // }
            //println("")
            val comb = genComb(byteArray, ar.indexOf('?'), values)
            sum = sum + BigInt(comb)
            println("Progress : " + (progress + 1) + " / " + lines.length)
        }
        val after = System.currentTimeMillis
        println("Elapsed = " + (after-before) + " ms")
        println("Sum = " + sum)
    }
}
