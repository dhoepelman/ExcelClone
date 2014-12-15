package scalaExcel.GUI.data

import scalaExcel.model.Sheet
import scalaExcel.GUI.data.DataWindow.Bounds

abstract class TableMutations

case object LayOutTable extends TableMutations
case class AddColumns(count: Int, index: Int) extends TableMutations
case class AddRows(count: Int, index: Int) extends TableMutations
case class RemoveColumns(count: Int, index: Int) extends TableMutations
case class RemoveRows(count: Int, index: Int) extends TableMutations
case class SlideWindowBy(offsets: Bounds) extends TableMutations
case class SlideWindowTo(bounds: Bounds) extends TableMutations
case class UpdateColumnOrder(permutations: Map[Int, Int]) extends TableMutations
case class UpdateContents(sheet: Sheet) extends TableMutations
case class ResizeColumn(columnIndex: Int, width: Double)extends TableMutations
case object Reset extends TableMutations