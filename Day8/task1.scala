import scala.io.Source

object D8T1 {
    class Pair(var lhs: String, var rhs: String)
    def main(args: Array[String]) = {
        val lines = Source.fromFile("input.txt").getLines.toArray
        val RL = lines(0)
        var move = 0
        var m = scala.collection.mutable.Map[String, Pair]()
        for(i <- 2 until lines.length) {
            val eq = lines(i).split('=')
            val lhs = eq(1).substring(eq(1).indexOf('(') + 1, eq(1).indexOf(',')).trim()
            val rhs = eq(1).substring(eq(1).indexOf(',') + 1, eq(1).indexOf(')')).trim()
            m(eq(0).trim()) = new Pair(lhs, rhs)
        }
        var found = false
        var steps = 0
        var myInstr = "AAA"
        while(found == false) {
            if(RL(move) == 'R') {
                myInstr = m(myInstr).rhs
            } else {
                myInstr = m(myInstr).lhs
            }
            move = move + 1
            steps = steps + 1
            if (move >= RL.length) {
                move = 0
            }
            if (myInstr == "ZZZ") {
                found = true
            }
        }
        println("Steps = " + steps)
    }
}
