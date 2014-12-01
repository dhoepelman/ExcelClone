package scalaExcel.GUI.data

import scalaExcel.CellPos
import scalaExcel.model.Styles
import scalafx.scene.paint.Color

/**
 * Usage cases for the data window
 */
abstract class ModelMutations

case class ReorderColumns(permutations: Map[Int, Int]) extends ModelMutations
case class SortColumn(column: Int, ascending: Boolean) extends ModelMutations
case class ChangeFormula(index: CellPos, expression: String) extends ModelMutations
case class ChangeBackground(index: CellPos, color: Color) extends ModelMutations
case class ChangeColor(index: CellPos, color: Color) extends ModelMutations
case class UpdateModelWindow(window: DataWindow) extends ModelMutations

