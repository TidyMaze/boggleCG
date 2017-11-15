/* solution for https://www.codingame.com/ide/puzzle/boggle */

import math._
import scala.util._

case class Coord(var x: Int, var y: Int) {
    def add(offset: (Int, Int)): Coord = {
        this.x += offset._1
        this.y += offset._2
        return this
    }
}

object Boggle {
    val gridSize = 4
    val offsets = List(
        (-1,-1), (-1,0), (-1,1),
        (0,-1), /* ,*/   (0,1), 
        (1,-1), (1,0), (1,1), 
    )
    
    def inGrid(pair: Coord): Boolean = {
        return pair.x >= 0 && pair.x < gridSize && pair.y >= 0 && pair.y < gridSize
    }
}

class BoggleSolver {
    
    def existingNeighbors(from: Coord) = {
        Boggle.offsets.map(from.copy().add(_)).filter(Boggle.inGrid)
    }
    
    def isWinning(grid: Array[Array[Char]], word: String, coord: Coord): Boolean = {
        if(word.isEmpty) return true
        if(grid(coord.y)(coord.x) != word(0)) return false
        var newGrid = grid.map(_.clone)
        newGrid(coord.y)(coord.x) = '.'
        existingNeighbors(coord).exists(isWinning(newGrid, word.substring(1), _))
    }
    
    def canBeFound(grid: Array[Array[Char]], word: String): Boolean = {
        val allCoords = for(y <- 0 until Boggle.gridSize; x <- 0 until Boggle.gridSize) yield Coord(x,y)
        allCoords.exists(isWinning(grid, word, _))
    }
}

object Solution extends App {
    val bs = new BoggleSolver()
    val grid = Array.ofDim[Char](Boggle.gridSize,Boggle.gridSize)

    for(num <- 0 until Boggle.gridSize){
        grid(num) = readLine.toArray
        Console.err.println(grid(num))
    }
    
    val n = readInt
    for(i <- 0 until n) {
        val w = readLine
        println(bs.canBeFound(grid, w))
    }
}
