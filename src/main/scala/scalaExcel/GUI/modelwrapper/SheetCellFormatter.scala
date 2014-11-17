package scalaExcel.GUI.modelwrapper

class SheetCellFormatter(method_ : SheetCell => String) {
  val method = method_

  //TODO expand with predicates
  def format(cell: SheetCell): String = method_(cell)

}

object SheetCellFormatter {
  private val defaultMethod = (cell: SheetCell) =>
    if (cell == null || cell.evaluated == null) ""
    else cell.evaluated.toString

  def default: SheetCellFormatter = new SheetCellFormatter(defaultMethod)
}