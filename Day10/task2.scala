import scala.io.Source

object D10T2 {
    class Pair(var x: Int, var y: Int)

    def fill(p: Pair, map : Array[Array[Char]], visited: scala.collection.mutable.ArrayBuffer[Pair]) : Boolean = {
        if(visited.find(pp => (pp.x == p.x && pp.y == p.y)) != None) {
            return true
        }
        visited += p
        val newPos = Array[Pair](Pair(p.x - 1, p.y), Pair(p.x + 1, p.y), Pair(p.x, p.y - 1), Pair(p.x, p.y + 1))
        var me = map(p.y)(p.x)
        if (me == '.') {
            map(p.y)(p.x) = 'O'
            me = 'O'
        }
        for((pos, i) <- newPos.zipWithIndex) {
            if(pos.x >= 0 && pos.y >= 0 && pos.x < map(0).length && pos.y < map.length) {
                if(map(pos.y)(pos.x) == '.') {
                    map(pos.y)(pos.x) = 'O'
                    fill(pos, map, visited)
                }
                val c = map(pos.y)(pos.x)
                val canGoUpDown = (c == '|' || c == 'J' || c == '7' || c == 'F' || c == 'L') && (me == '.' || me == 'O' || me == '|' || me == 'J' || me == '7' || me == 'F' || me == 'L')
                if(i == 0 && c == '-') {
                    //fill(pos, map, visited)
                } else if (i == 1 && c == '-') {
                    //fill(pos, map, visited)
                } else if (i == 2 && canGoUpDown) {
                    //fill(pos, map, visited)
                } else if (i == 3 && canGoUpDown) {
                    //fill(pos, map, visited)
                }
            }
        }
        true
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

    def findFill(pipe: Char) : Char = {
        if(pipe == '|') {
            return '|'
        }
        return 'X'
    }

    def resize(old: Array[Array[Char]]) : Array[Array[Char]] = {
        var res = new Array[Array[Char]](old.length * 2)
        for(i <- 0 until res.length) {
            res(i) = new Array[Char](old(0).length * 2)
            for(j <- 0 until res(i).length) {
                res(i)(j) = '.'
            }
        }
        for(y <- 0 until old.length) {
            for(x <- 0 until old(y).length) {
                res(y*2)(x*2) = old(y)(x)
                val myC = old(y)(x)
                if(myC != '.') {
                    val up = Pair(x * 2, y * 2 - 1)
                    if(up.y >= 0) {
                        res(up.y)(up.x) = findFill(myC)
                    }
                    //val down = Pair(j + 1, i)
                }
            }
        }
        return res
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
        var nmap = resize(map)
        var visited = new scala.collection.mutable.ArrayBuffer[Pair](0)
        for(i <- 0 until map.length) {
            if(map(i)(0) == '.') {
                //fill(Pair(0, i), map, visited)
            }
            if(map(i)(map(i).length - 1) == '.') {
                //fill(Pair(map(i).length - 1, i), map, visited)
            }
        }
        for(i <- 0 until map(0).length) {
            if(map(0)(i) == '.') {
                //fill(Pair(i, 0), map, visited)
            }
            if(map(map.length - 1)(i) == '.') {
                //fill(Pair(i, map.length - 1), map, visited)
            }
        }
        printMap(nmap)
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
        //assert(calculateDist("example1.txt") == 4, "example1.txt != 4")
       // assert(calculateDist("example2.txt") == 8, "example2.txt != 8")
       // assert(calculateDist("example3.txt") == 10, "example3.txt != 10")
       // println("Dist = " + calculateDist("input.txt"))
    }
}
