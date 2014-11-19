package scalaExcel.GUI.data

/**
 * Types of manipulations of the data window
 */
abstract class DataWindowChanges

case class RefreshWindow() extends DataWindowChanges

case class MoveWindowBy(offsets: (Int, Int, Int, Int)) extends DataWindowChanges

case class MoveWindowTo(bounds: (Int, Int, Int, Int)) extends DataWindowChanges

case class NewColumn(position: Int) extends DataWindowChanges

case class NewRow() extends DataWindowChanges

case class ReorderColumns(permutations: Map[Int, Int]) extends DataWindowChanges
