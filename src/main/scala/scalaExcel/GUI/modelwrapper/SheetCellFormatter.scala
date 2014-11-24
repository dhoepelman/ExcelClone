package scalaExcel.GUI.modelwrapper

class SheetCellFormatter(val method : SheetCell => String) {
  //TODO expand with predicates
  def format(cell: SheetCell): String = method(cell)

}

object SheetCellFormatter {
  private val defaultMethod = (cell: SheetCell) =>
    if (cell == null || cell.evaluated == null) ""
    else cell.evaluated.toString

  def default: SheetCellFormatter = new SheetCellFormatter(defaultMethod)
}