package scalaExcel.model

import scalafx.scene.paint.Color
import scalaExcel.CellPos

/**
 * Types of inputs to the data model
 */
sealed trait ModelMutations
case class SetFormula(pos : CellPos, f: String) extends ModelMutations
case class EmptyCell(pos : CellPos) extends ModelMutations
case class CopyCell(from : CellPos, to : CellPos) extends ModelMutations
case class CutCell(from : CellPos, to : CellPos) extends ModelMutations
case class SetColor(pos : CellPos, c: Color) extends ModelMutations
case class SetBackground(pos : CellPos, c: Color) extends ModelMutations
case class SortColumn(x: Int, asc: Boolean) extends ModelMutations
case object Redo extends ModelMutations
case object Undo extends ModelMutations
case object Refresh extends ModelMutations
