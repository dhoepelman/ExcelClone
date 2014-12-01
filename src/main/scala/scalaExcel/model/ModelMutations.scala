package scalaExcel.model

import scalafx.scene.paint.Color

/**
 * Types of inputs to the data model
 */
sealed trait ModelMutations
case class SetFormula(x: Int, y: Int, f: String) extends ModelMutations
case class EmptyCell(x : Int, y: Int) extends ModelMutations
case class CopyCell(from : CellPos, to : CellPos) extends ModelMutations
case class CutCell(from : CellPos, to : CellPos) extends ModelMutations
case class SetColor(x: Int, y: Int, c: Color) extends ModelMutations
case class SetBackground(x: Int, y: Int, c: Color) extends ModelMutations
case class SortColumn(x: Int, asc: Boolean) extends ModelMutations
case object Refresh extends ModelMutations
