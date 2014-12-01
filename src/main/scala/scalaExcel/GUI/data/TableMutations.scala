package scalaExcel.GUI.data

import scalaExcel.CellPos
import scalaExcel.model.Styles
import scalaExcel.formula.Value

abstract class TableMutations

case class RefreshTable() extends TableMutations
case class UpdateColumnOrder(permutations: Map[Int, Int]) extends TableMutations
case class UpdateContents(cellContents: Iterable[(CellPos, String, Value, Styles)]) extends TableMutations
case class UpdateWindow(dataWindow: DataWindow) extends TableMutations
case class ResizeColumn(columnIndex: Int, width: Double)extends TableMutations

