import scala.io.Source

object D8T2 {
    class Pair(var lhs: String, var rhs: String)

    def gcd(lhs: BigInt, rhs: BigInt) : BigInt = {
        if (rhs == 0) {
            return lhs
        }
        return gcd(rhs, lhs % rhs)
    }

    def lcm(lhs: BigInt, rhs: BigInt) : BigInt = {
        return (lhs / gcd(lhs, rhs)) * rhs
    }

    def main(args: Array[String]) = {
        val lines = Source.fromFile("input.txt").getLines.toArray
        val RL = lines(0)
        var move = 0
        var m = scala.collection.mutable.Map[String, Pair]()
        var start = new Array[String](0)
        for(i <- 2 until lines.length) {
            val eq = lines(i).split('=')
            val lhs = eq(1).substring(eq(1).indexOf('(') + 1, eq(1).indexOf(',')).trim()
            val rhs = eq(1).substring(eq(1).indexOf(',') + 1, eq(1).indexOf(')')).trim()
            m(eq(0).trim()) = new Pair(lhs, rhs)
            if (eq(0).trim()(2) == 'A') {
                start :+= eq(0).trim()
            }
        }
        var repeated = new Array[BigInt](start.length)
        for(i <- 0 until repeated.length) {
            repeated(i) = BigInt(0)
        }
        val startedConst = start.clone()
        var found = false
        var steps = BigInt(0)
        while(found == false) {
            val mv = RL(move)
            mv match {
                case 'R' => start = start.map{ case x => m(x).rhs }
                case 'L' => start = start.map{ case x => m(x).lhs }
            }
            move = move + 1
            steps = steps + 1
            if (move >= RL.length) {
                move = 0
            }
            for((a, i) <- start.zipWithIndex) {
                if (repeated(i) == 0 && a(2) == 'Z') {
                    repeated(i) = steps
                }
            }
            if (repeated.forall(_ != BigInt(0))) {
                found = true
            }
        }
        steps = 1
        for(v <- repeated) {
            steps = lcm(steps, v)
        }
        println("Steps = " + steps)
    }
}
