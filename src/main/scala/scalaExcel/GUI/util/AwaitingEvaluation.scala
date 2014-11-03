package scalaExcel.GUI.util

class AwaitingEvaluation(expr_ : String) {
  override def toString: String = "Awaiting evaluation of " + expr_
}
