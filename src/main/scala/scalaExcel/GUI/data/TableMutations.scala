package scalaExcel.GUI.data

import scalaExcel.model.Styles

abstract class TableMutations

case class RefreshTable() extends TableMutations
case class UpdateColumnOrder(permutations: Map[Int, Int]) extends TableMutations
case class UpdateContents(cellContents: Iterable[((Int, Int), String, Any, Styles)]) extends TableMutations
case class UpdateWindow(dataWindow: DataWindow) extends TableMutations

