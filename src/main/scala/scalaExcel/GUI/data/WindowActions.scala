package scalaExcel.GUI.data

import scalaExcel.CellPos
import scalaExcel.model.Styles

/**
 * Usage cases for the data window
 */
abstract class WindowActions

case class ReorderColumns(permutations: Map[Int, Int]) extends WindowActions
case class SortRows(sortColumn: Int, sortAscending: Boolean) extends WindowActions
case class ChangeCellExpression(index: CellPos, expression: String) extends WindowActions
case class NewWindow(window: DataWindow) extends WindowActions
case class ChangeCellStyle(index: CellPos, style: Styles) extends WindowActions

