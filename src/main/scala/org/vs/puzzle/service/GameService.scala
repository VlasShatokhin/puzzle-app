package org.vs.puzzle.service

import org.vs.puzzle.PuzzleTable
import org.vs.puzzle.PuzzleTable.Position

import scala.annotation.tailrec

class GameService {

  def findNextPosition(positionFrom: Position,
                       puzzleTable: PuzzleTable): Option[Position] = {

    val vertex = puzzleTable.vertex
    def getPotential: List[Position] = positionFrom match {
      case (1, 1) => List((1, 2), (2, 1))
      case (1, y) if y == vertex => List((2, y), (1, y - 1))
      case (x, 1) if x == vertex => List((x - 1, 1), (x, 2))
      case (x, y) if x == vertex && y == vertex => List((x - 1, y), (x, y - 1))
      case (x, y) if y == vertex => List((x + 1, y), (x - 1, y), (x, y - 1))
      case (x, y) if x == vertex => List((x - 1, y), (x, y - 1), (x, y + 1))
      case (1, y) => List((2, y), (1, y + 1), (1, y - 1))
      case (x, 1) => List((x, 2), (x + 1, 1), (x - 1, 2))
      case (x, y) => List((x + 1, y), (x - 1, y), (x, y + 1), (x, y - 1))
    }

    getPotential find puzzleTable.nonDefined
  }

  @tailrec
  private def getPosition(index: Int, vertex: Int, row: Int = 1): Position =
    if (index <= vertex) (row, index)
    else getPosition(index - vertex, row + 1)
}
