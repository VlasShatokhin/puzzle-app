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

  def getIndex(position: Position): Int = position match {
    case (x, y) => x * y
  }
}

class PuzzleTable(val vertex: Int, generator: Int => List[Option[Int]]) {

  require(vertex > 1, "table dimension should be > 1")

  private val list: List[Option[Int]] = generator(vertex)

  def nonDefined(position: Position): Boolean = list {
    getIndex(position) - 1
  } isEmpty

  def reposition(from: Position, to: Position): PuzzleTable =
    PuzzleTable(vertex, list
      .updated(getIndex(from), None)
      .updated(getIndex(to), list(getIndex(from))))

  @tailrec
  private def toTable[T](list: List[T], vertex: Int, table: List[List[T]] = List()): List[List[T]] =
    if (list.size <= vertex) table :+ list
    else toTable(list.drop(vertex), vertex, table :+ list.take(vertex))

  override def toString: String = {
    for (row <- toTable(list, vertex)) yield
      row mkString " "
  } mkString "\n"

}
