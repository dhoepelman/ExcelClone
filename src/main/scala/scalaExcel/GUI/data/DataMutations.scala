package scalaExcel.GUI.data

/**
 * Types of manipulations of the data window
 */
abstract class DataChanges

case class RefreshData() extends DataChanges

case class ModifyFormulaAt(index: (Int, Int), formula: String) extends DataChanges

case class UpdateContents(cellContents: Iterable[((Int, Int), String, Any)]) extends DataChanges

case class SlideWindowBy(offsets: (Int, Int, Int, Int)) extends DataChanges

case class MoveWindowTo(bounds: (Int, Int, Int, Int)) extends DataChanges

case class AddNewColumn(position: Int) extends DataChanges

case class AddNewRow() extends DataChanges

case class ReorderColumns(permutations: Map[Int, Int]) extends DataChanges

case class SortRows(sortColumn: Int, sortAscending: Boolean) extends DataChanges