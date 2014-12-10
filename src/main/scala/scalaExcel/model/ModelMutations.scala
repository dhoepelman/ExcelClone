package scalaExcel.model

import scalafx.scene.paint.Color
import scalaExcel.CellPos

/**
 * Types of inputs to the data model
 */
sealed trait ModelMutations
case class SetFormula(pos : CellPos, f: String) extends ModelMutations
case class EmptyCell(pos : Traversable[CellPos]) extends ModelMutations
case class CopyCell(from : CellPos, to : CellPos) extends ModelMutations
case class CutCell(from : CellPos, to : CellPos) extends ModelMutations
case class SetColor(pos : Traversable[CellPos], c: Color) extends ModelMutations
case class SetBackground(pos : Traversable[CellPos], c: Color) extends ModelMutations
case class SortColumn(x: Int, asc: Boolean) extends ModelMutations
case object Redo extends ModelMutations
case object Undo extends ModelMutations
case object Refresh extends ModelMutations
case class SetSheet(values: Map[(Int,Int),String], styles: Map[(Int,Int), Styles]) extends ModelMutations
case class Add(toRows: Boolean, count: Int, index: Int) extends ModelMutations
case class Remove(fromRows: Boolean, count: Int, index: Int) extends ModelMutations
case class ReorderColumns(permutations: Map[Int, Int]) extends ModelMutations