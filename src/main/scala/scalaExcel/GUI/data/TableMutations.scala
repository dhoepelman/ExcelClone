package scalaExcel.GUI.data

import scalaExcel.model.Sheet
import scalaExcel.GUI.data.DataWindow.Bounds

abstract class TableMutations

case class LayOutTable(width: Double, height: Double, cellSize: Double) extends TableMutations
case class AddNewColumn(position: Int) extends TableMutations
case class AddNewRow(position: Int) extends TableMutations
case class SlideWindowBy(offsets: Bounds) extends TableMutations
case class SlideWindowTo(bounds: Bounds) extends TableMutations
case class UpdateColumnOrder(permutations: Map[Int, Int]) extends TableMutations
case class UpdateContents(sheet: Sheet) extends TableMutations
case class ResizeColumn(columnIndex: Int, width: Double)extends TableMutations

