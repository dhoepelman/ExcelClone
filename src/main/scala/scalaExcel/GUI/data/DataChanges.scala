package scalaExcel.GUI.data

/**
 * Types of manipulations of the data window
 */
abstract class DataChanges

case class RefreshData() extends DataChanges

case class WindowChanged(newWindow: DataWindow) extends DataChanges

case class ContentsChanged(cellContents: Iterable[((Int, Int), String, Any)]) extends DataChanges
