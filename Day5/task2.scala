import scala.io.Source
import scala.math.BigInt

object D5T2 {

    def map(value: BigInt,range: String) : BigInt = {
        val ranges = range.split(' ').filter(_.nonEmpty).map(BigInt(_))
        val destRange = ranges(1)
        val destSrc = ranges(0)
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

        type Point = (BigInt,BigInt)

        // Parse input
        val seedsFile = lines(seeds_idx).substring(6).split(' ').filter(_.nonEmpty).map(BigInt(_))
        var i = 25000000 // Let's go bruteforce!!!
        var found = false
        while(found == false) {
            val loc = findMap(i, h_to_l, lines.length, lines)
            val humi = findMap(loc, t_to_h, h_to_l, lines)
            val temp = findMap(humi, l_to_t, t_to_h, lines)
            val light = findMap(temp, w_to_l, l_to_t, lines)
            val water = findMap(light, f_to_w, w_to_l, lines)
            val fer = findMap(water, s_to_f, f_to_w, lines)
            val soil = findMap(fer, s_to_s, s_to_f, lines)
            for(s <- soil) {
                for(j <- 0 until seedsFile.length by 2) {
                    if(s >= seedsFile(j) && s < seedsFile(j) + seedsFile(j+1)) {
                        println("Seed = " + s)
                        println("Location = " + i)
                        found = true
                    }
                }
            }
            i += 1
            println(i)
        }
    }
}
