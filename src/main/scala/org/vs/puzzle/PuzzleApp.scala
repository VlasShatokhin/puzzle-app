package org.vs.puzzle

import org.vs.puzzle.PuzzleTable.Position
import org.vs.puzzle.service.GameService

import scala.io.StdIn
import scala.language.postfixOps
import scala.util.{Failure, Success, Try}

/**
  * 15Puzzle game.
  *
  * Rows and columns have starting position 1
  */
object PuzzleApp {

  def main(args: Array[String]): Unit = {

    val gameService = new GameService()

    println("""
              |Welcome to NPuzzle game!
              |Please enter size of a vertex, so you'll have an n * n table:
            """.stripMargin)
    val vertex: Int = Try {
      StdIn.readInt()
    } match {
      case Success(n) => n
      case Failure(x) => throw x
    }

    var puzzle = PuzzleTable(vertex,
      v => (1 until v * v).map(Some(_)) :+ None toList)

    while (true) {
      println(s"""
                 |$puzzle
                 |
                 |Enter a coordinate in from of x,y:""".stripMargin)
      val from: Position = Try {
        val positionString = StdIn.readLine().split(",")
        (positionString(0).toInt, positionString(1).toInt)
      } match {
        case Success(pos) => pos
        case Failure(x) => throw x
      }

      gameService.findNextPosition(from, puzzle) match {
        case Some(to) => puzzle = puzzle.reposition(from, to)
        case None => println("No spot available!")
      }
    }
  }

}
