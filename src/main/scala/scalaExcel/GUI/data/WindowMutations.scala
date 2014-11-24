package scalaExcel.GUI.data

/**
 * Types of manipulations of the data window
 */
abstract class WindowMutations

case class RefreshWindow() extends WindowMutations
case class AddNewColumn(position: Int) extends WindowMutations
case class AddNewRow() extends WindowMutations
case class SlideWindowBy(offsets: (Int, Int, Int, Int)) extends WindowMutations
case class SlideWindowTo(bounds: (Int, Int, Int, Int)) extends WindowMutations
