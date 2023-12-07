import scala.io.Source

object D7T1 {
    def getCardValue(card: Char): Int = {
        if (card >= '0' && card <= '9') {
            return (card - '0').toInt
        }
        else if (card == 'T') {
            return 10
        }
        else if (card == 'J') {
            return 11
        }
        else if (card == 'Q') {
            return 12
        }
        else if (card == 'K') {
            return 13
        }
        return 14
    }

    def getValue(h: String) : Int = {
        var m = scala.collection.mutable.Map[Char, Int]()
        for(c <- h) {
            if (m.contains(c)) {
                m(c) = m(c) + 1
            }
            else {
                m(c) = 1
            }
        }
        if(m.size == 1) {
            return 10
        }
        if(m.size == 2 && (m.head._2 == 1 || m.head._2 == 4)) {
            return 8
        }
        if(m.size == 2 && (m.head._2 == 2 || m.head._2 == 3)) {
            return 6
        }
        var tree_of_kind = false
        if(m.size == 3) {
            for((key, value) <- m) {
                if(value == 3) {
                    tree_of_kind = true
                }
            }
        }
        if (tree_of_kind) {
            return 4
        }
        var cnt = 0
        for((key, value) <- m) {
            if(value == 2) {
                cnt = cnt + 1
            }
        }
        if (cnt == 2) {
            return 2
        } else if (cnt == 1) {
            return 1
        }
        return 0
    }

    def sortOrder(lhs: String, rhs: String) : Boolean = {
        val left = lhs.split(' ')(0)
        val right = rhs.split(' ')(0)
        val l_val = getValue(left)
        val r_val = getValue(right)
        if (l_val < r_val) {
            return true
        }
        else if (l_val == r_val) {
            var idx = 0
            var lv = getCardValue(left(idx))
            var rv = getCardValue(right(idx))
            while(lv == rv) {
                idx = idx + 1
                lv = getCardValue(left(idx))
                rv = getCardValue(right(idx))
            }
            return lv < rv
        }
        false
    }
    def main(args: Array[String]) = {
        val lines = Source.fromFile("input.txt").getLines.toArray
        val maxRank = lines.length
        var winnings = 0

        val sorted = lines.sortWith(sortOrder)
        for((l, i) <- sorted.zipWithIndex) {
            winnings = winnings + l.split(' ')(1).toInt * (i + 1)
        }
        println("Winnings = " + winnings)
    }
}
