package scalaExcel.GUI.model

import scalaExcel.GUI.util.{CircularEvaluation, CSSHelper, ErroneousEvaluation}

class SheetCellStylist(fixed_ : String, method_ : SheetCell => String) {

  val method = method_
  val fixed = if (fixed_ == null) "" else fixed_

  //TODO expand with predicates
  def style(cell: SheetCell): String =
    if (cell == null) ""
    else cell.evaluated match {
      case x: ErroneousEvaluation => CSSHelper.errorStyle
      case x: CircularEvaluation => CSSHelper.errorStyle
      case _ => if (method_ != null) method(cell) else fixed
    }
}

object SheetCellStylist {

  def default: SheetCellStylist = new SheetCellStylist("", null)
}