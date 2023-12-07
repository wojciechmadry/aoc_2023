import scala.io.Source

object D7T2 {
    def getCardValue(card: Char): Int = {
        if (card >= '0' && card <= '9') {
            return (card - '0').toInt
        }
        else if (card == 'T') {
            return 10
        }
        else if (card == 'J') {
            return 0
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
        val jokers = h.count(_ == 'J')
        for(c <- h) {
            if (c != 'J') {
                if (m.contains(c)) {
                    m(c) = m(c) + 1
                }
                else {
                    m(c) = 1
                }
            }
        }
        if(m.size <= 1) {
            return 10
        }
        if((m.size <= 2 && (m.head._2 == 1 || m.head._2 == 4)) ||
            (m.size <= 2 && (m.head._2 + jokers == 1 || m.head._2 + jokers == 4))) {
            return 8
        }
        if(m.size == 2 && (m.head._2 == 2 || m.head._2 == 3)) {
            return 6
        }
        var tree_of_kind = false
        if(m.size == 3) {
            for((key, value) <- m) {
                if(value + jokers >= 3) {
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
        if (cnt + jokers >= 2) {
            return 2
        } else if (cnt + jokers >= 1) {
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
    def doAssert(hand: String, req : Int) = {
        val value = getValue(hand)
        assert(value == req, "Hand: " + hand + " should equal '" + req + "' but it is '" + value + "'")
    }

    def main(args: Array[String]) = {
        val lines = Source.fromFile("input.txt").getLines.toArray
        val maxRank = lines.length
        var winnings = 0
        doAssert("AAAAA", 10)
        doAssert("AAAJA", 10)
        doAssert("AAJJA", 10)
        doAssert("AJJJA", 10)
        doAssert("JJJJA", 10)
        doAssert("JJJJJ", 10)
        doAssert("AAAAB", 8)
        doAssert("AAABJ", 8)
        doAssert("AABJJ", 8)
        doAssert("AJBJJ", 8)
        doAssert("2333J", 8)
        doAssert("AJBJJ", 8)
        doAssert("J3332", 8)
        doAssert("2JJ32", 8)
        doAssert("2J3J2", 8)
        doAssert("23332", 6)
        doAssert("2J332", 6)
        doAssert("23J32", 6)
        doAssert("233J2", 6)
        doAssert("TTT98", 4)
        doAssert("TTJ98", 4)
        doAssert("JTJ98", 4)
        doAssert("234JJ", 4)
        doAssert("23355", 2)
        doAssert("2AA55", 2)
        doAssert("2347J", 1)
        doAssert("2947J", 1)
        doAssert("2J479", 1)
        doAssert("23456", 0)
        val sorted = lines.sortWith(sortOrder)
        for((l, i) <- sorted.zipWithIndex) {
            winnings = winnings + l.split(' ')(1).toInt * (i + 1)
        }
        println("Winnings = " + winnings)
    }
}
