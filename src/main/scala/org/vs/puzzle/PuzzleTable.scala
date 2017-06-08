package org.vs.puzzle

import org.vs.puzzle.PuzzleTable._

import scala.annotation.tailrec
import scala.language.postfixOps

object PuzzleTable {

  type Position = (Int, Int)

  def apply(vertex: Int, generator: Int => List[Option[Int]]): PuzzleTable =
    new PuzzleTable(vertex, generator)

  def apply(vertex: Int, list: List[Option[Int]]): PuzzleTable =
    new PuzzleTable(vertex, _ => list)

  def getIndex(position: Position, vertex: Int): Int = position match {
    case (x, y) => (x - 1) * vertex + y - 1
  }

  @tailrec
  final def getPosition(index: Int, vertex: Int, row: Int = 1): Position =
    if (index <= vertex) (row, index)
    else getPosition(index - vertex, vertex, row + 1)
}

class PuzzleTable(val vertex: Int, generator: Int => List[Option[Int]]) {

  require(vertex > 1, "table dimension should be > 1")

  private val list: List[Option[Int]] = generator(vertex)

  def nonDefined(position: Position): Boolean = list {
    getIndex(position, vertex)
  } isEmpty

  def reposition(from: Position, to: Position): PuzzleTable =
    PuzzleTable(vertex, list
      .updated(getIndex(from, vertex), None)
      .updated(getIndex(to, vertex), list(getIndex(from, vertex))))

  def positionByValue(value: Int): Option[Position] = list.zipWithIndex find {
    case (v, _) => v contains value
  } map {
    case (_, position) => getPosition(position + 1, vertex)
  }

  @tailrec
  private def toTable[T](list: List[T], vertex: Int, table: List[List[T]] = List()): List[List[T]] =
    if (list.size <= vertex) table :+ list
    else toTable(list.drop(vertex), vertex, table :+ list.take(vertex))

  override def toString: String = {
    for (row <- toTable(list, vertex)) yield
      row mkString " "
  } mkString "\n"

}
