import scala.io.Source

object D10T2 {
    class Pair(var x: Int, var y: Int)

    def generateDir(c : Char, myPose : Pair, lastC: Char) : Array[Pair] =  {
        var result = new Array[Pair](0)
        val left = Pair(myPose.x - 1, myPose.y)
        val right = Pair(myPose.x + 1, myPose.y)
        val up = Pair(myPose.x, myPose.y - 1)
        val down = Pair(myPose.x, myPose.y + 1)
        if(c == 'S' || c == '.' || c == 'O') {
            result :+= left
            result :+= right
            result :+= up
            result :+= down
        }
        else if(c == '|') {
            result :+= up
            result :+= down
        }
        else if(c == '-') {
            result :+= left
            result :+= right
        }
        else if(c == 'L') {
            result :+= up
            result :+= right
        }
        else if(c == 'J') {
            result :+= up
            result :+= left
        }
        else if(c == '7') {
            result :+= left
            result :+= down
        }
        else if(c == 'F') {
            result :+= down
            result :+= right
        }
        return result
    }

    def generateConnection(c : Char) : Array[Char] = {
        var result = new Array[Char](0)

        return result
    }

    def fill(p: Pair, map : Array[Array[Char]], visited: scala.collection.mutable.ArrayBuffer[Pair],
        dir : Array[Pair], connect: Array[Char], lines: Array[Array[Char]]) : Boolean = {
        var me = map(p.y)(p.x)
        if(visited.find(pp => (pp.x == p.x && pp.y == p.y)) != None) {
            //return false
            var atLeastOne = false
            for(v <- visited) {
                if((v.x == 0 || v.x == lines(0).length - 1 || v.y == 0 || v.y == lines.length - 1) && (lines(v.y)(v.x) == 'O' || lines(v.y)(v.x) == '.')) {
                    atLeastOne = true
                }
            }
            return atLeastOne && (me == 'O' || me == '.')
        }
        visited += p
        var out = false
        for((pos, i) <- dir.zipWithIndex) {
            if(pos.x >= 0 && pos.y >= 0 && pos.x < map(0).length && pos.y < map.length) {
                val myC = map(pos.y)(pos.x)
                if((myC == '.' || myC == 'O') && (pos.x == 0 || pos.x == lines(0).length - 1 || pos.y == 0 || pos.y == lines.length - 1)) {
                    map(pos.y)(pos.x) = 'O'
                    out = true
                }
                val canFill = fill(pos, map, visited, generateDir(myC, pos, me), generateConnection(myC), lines)
                if(canFill && (me == '.' || me == 'O')) {
                    map(p.y)(p.x) = 'O'
                }
            }
        }
        out
    }

    def printMap(map: Array[Array[Char]]) = {
        println("=======================")
        for(row <- map) {
            for(col <- row) {
                var v = col
               // if(v != '.' && v != 'O') {
               //     v = 'X'
               // }
                print(v + " ")
            }
            println("")
        }
        println("=======================")
    }


    def calculateDist(filename: String): Int = {
        val lines = Source.fromFile(filename).getLines.toArray
        var map = new Array[Array[Char]](lines.length)
        for((l, i) <- lines.zipWithIndex) {
            map(i) = new Array[Char](l.length)
            for((c, j) <- l.zipWithIndex) {
                map(i)(j) = c
            }
        }
       /* for(i <- 0 until map.length) {
            if(map(i)(0) == '.') {
                val pos = Pair(0, i)
                fill(pos, map, visited, generateDir('.', pos, '.'), generateConnection('.'))
            }
            if(map(i)(map(i).length - 1) == '.') {
                val pos = Pair(map(i).length - 1, i)
                fill(pos, map, visited, generateDir('.', pos, '.'), generateConnection('.'))
            }
        }
        for(i <- 0 until map(0).length) {
            if(map(0)(i) == '.') {
                val pos = Pair(i, 0)
                fill(pos, map, visited, generateDir('.', pos, '.'), generateConnection('.'))
            }
            if(map(map.length - 1)(i) == '.') {
                val pos = Pair(i, map.length - 1)
                fill(pos, map, visited, generateDir('.', pos, '.'), generateConnection('.'))
            }
        }*/
        for(i <- 0 until map.length) {
            for(j <- 0 until map(i).length) {
                if(map(i)(j) == '.') {
                    val pos = Pair(j, i)
                    var visited = new scala.collection.mutable.ArrayBuffer[Pair](0)
                    fill(pos, map, visited, generateDir('.', pos, '.'), generateConnection('.'), map)

                }
            }
       }
        printMap(map)
        var enclosed = 0
        for(row <- map) {
            for(c <- row) {
                if(c == '.') {
                    enclosed = enclosed + 1
                }
            }
        }
        return enclosed
    }
    def main(args: Array[String]) = {
        assert(calculateDist("example.txt") == 4, "example.txt != 4")
        assert(calculateDist("example1.txt") == 4, "example.txt != 4")
        assert(calculateDist("example2.txt") == 8, "example2.txt != 8")
        assert(calculateDist("example3.txt") == 8, "example3.txt != 8")
        assert(calculateDist("example4.txt") == 8, "example4.txt != 8")
        //println(calculateDist("input.txt")) // 250 < result < 350 <- Estimated from aoc web AND (!= 280,341)
    }
}
