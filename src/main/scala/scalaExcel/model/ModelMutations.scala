package scalaExcel.model

import scalafx.scene.paint.Color

/**
 * Types of inputs to the data model
 */
sealed trait ModelMutations
case object Refresh extends ModelMutations
case class SetFormula(x: Int, y: Int, f: String) extends ModelMutations
case class SetStyle(x: Int, y: Int, s: Styles) extends ModelMutations
case class EmptyCell(x : Int, y: Int) extends ModelMutations
case class CopyCell(from : CellPos, to : CellPos) extends ModelMutations
case class CutCell(from : CellPos, to : CellPos) extends ModelMutationscase class SetStyle(x: Int, y: Int, s: Styles) extends ModelMutations
