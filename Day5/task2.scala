import scala.io.Source
import scala.math.BigInt

object D5T2 {
    def map(value: BigInt,range: String) : BigInt = {
        val ranges = range.split(' ').filter(_.nonEmpty).map(BigInt(_))
        val destRange = ranges(0)
        val destSrc = ranges(1)
        val rangeLength = ranges(2)
        if(value >= destSrc && value <= destSrc + rangeLength) {
            return destRange + value - destSrc
        }
        -1
    }

    def findMap(value: BigInt, lhs: Int, rhs: Int, lines: Array[String]) : Array[BigInt] = {
        var result =  new Array[BigInt](0)
        for (i <- lhs + 1 to rhs - 1) {
            val line = lines(i)
            if (!line.isEmpty) {
                val res = map(value, line)
                if (res != -1) {
                    result :+= res
                }
            }
        }
        if(result.isEmpty) {
            result :+= value
        }
        return result
    }

    def findMap(values: Array[BigInt], lhs: Int, rhs: Int, lines: Array[String]) : Array[BigInt] = {
        var result =  new Array[BigInt](0)
        for(value <- values) {
            val mapped = findMap(value, lhs, rhs, lines)
            result = Array.concat(result, mapped)
        }
        return result
    }

    def main(args: Array[String]) = {
        val lines = Source.fromFile("example.txt").getLines.toArray
        // Input indexes
        val seeds_idx = 0
        val s_to_s = lines.indexOf("seed-to-soil map:")
        val s_to_f = lines.indexOf("soil-to-fertilizer map:")
        val f_to_w = lines.indexOf("fertilizer-to-water map:")
        val w_to_l = lines.indexOf("water-to-light map:")
        val l_to_t = lines.indexOf("light-to-temperature map:")
        val t_to_h = lines.indexOf("temperature-to-humidity map:")
        val h_to_l = lines.indexOf("humidity-to-location map:")

        val const = BigInt(100000)
        // Parse input
        val seedsFile = lines(seeds_idx).substring(6).split(' ').filter(_.nonEmpty).map(BigInt(_))
        var min = BigInt("99999999999999999999")
        for(i <- 0 until seedsFile.length by 2) {
            var j = seedsFile(i)
            var last = BigInt("0")
            var changed = false
            while(j < seedsFile(i) + seedsFile(i+1)) {
                val s = j
                val soil = findMap(s, s_to_s, s_to_f, lines)
                val fer = findMap(soil, s_to_f, f_to_w, lines)
                val water = findMap(fer, f_to_w, w_to_l, lines)
                val light = findMap(water, w_to_l, l_to_t, lines)
                val temp = findMap(light, l_to_t, t_to_h, lines)
                val humi = findMap(temp, t_to_h, h_to_l, lines)
                val loc = findMap(humi, h_to_l, lines.length, lines)
                val loc_min = loc.min
                if(loc_min < min) {
                    min = loc_min
                }
                /*if(last != min && changed) {
                    j = j - const + 1
                    changed = false; 
                }
                if (last == min) {
                    j += const
                    changed = true
                } else {
                    j = j + 1
                }*/
                j = j + 1
                last = min
                
               // println("Progress : " + j + " / " + seedsFile(i) + seedsFile(i+1))
            }
        }
        println("Best location = " + min)
    }
}
