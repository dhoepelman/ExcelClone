package scalaExcel.GUI.data

abstract class TableMutations

case class RefreshTable() extends TableMutations
case class UpdateContents(cellContents: Iterable[((Int, Int), String, Any)]) extends TableMutations
case class UpdateWindow(dataWindow: DataWindow) extends TableMutations

