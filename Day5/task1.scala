import scala.io.Source
import scala.math.BigInt

object D5T1 {
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
        val lines = Source.fromFile("input.txt").getLines.toArray
        // Input indexes
        val seeds_idx = 0
        val s_to_s = lines.indexOf("seed-to-soil map:")
        val s_to_f = lines.indexOf("soil-to-fertilizer map:")
        val f_to_w = lines.indexOf("fertilizer-to-water map:")
        val w_to_l = lines.indexOf("water-to-light map:")
        val l_to_t = lines.indexOf("light-to-temperature map:")
        val t_to_h = lines.indexOf("temperature-to-humidity map:")
        val h_to_l = lines.indexOf("humidity-to-location map:")

        // Parse input
        val seeds = lines(seeds_idx).substring(6).split(' ').filter(_.nonEmpty).map(BigInt(_))
        var location = new Array[BigInt](seeds.length)

        for((s, i) <- seeds.zipWithIndex) {
            val soil = findMap(s, s_to_s, s_to_f, lines)
            val fer = findMap(soil, s_to_f, f_to_w, lines)
            val water = findMap(fer, f_to_w, w_to_l, lines)
            val light = findMap(water, w_to_l, l_to_t, lines)
            val temp = findMap(light, l_to_t, t_to_h, lines)
            val humi = findMap(temp, t_to_h, h_to_l, lines)
            val loc = findMap(humi, h_to_l, lines.length, lines)
            location(i) = loc.min
        }
        println("Best location = " + location.min)
    }
}
