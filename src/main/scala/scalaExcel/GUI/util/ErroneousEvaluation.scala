package scalaExcel.GUI.util

class ErroneousEvaluation(expr_ : String) {
  override def toString: String = "Error evaluating " + expr_
}
