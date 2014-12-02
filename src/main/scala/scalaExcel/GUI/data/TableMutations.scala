package scalaExcel.GUI.data

import scalaExcel.model.Sheet

abstract class TableMutations

case class RefreshTable() extends TableMutations
case class AddNewColumn(position: Int) extends TableMutations
case class AddNewRow(position: Int) extends TableMutations
case class SlideWindowBy(offsets: (Int, Int, Int, Int)) extends TableMutations
case class SlideWindowTo(bounds: (Int, Int, Int, Int)) extends TableMutations
case class UpdateColumnOrder(permutations: Map[Int, Int]) extends TableMutations
case class UpdateContents(sheet: Sheet) extends TableMutations
case class ResizeColumn(columnIndex: Int, width: Double)extends TableMutations

