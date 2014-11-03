package scalaExcel.GUI.util

class CircularEvaluation(expr_ : String) {
  override def toString: String = "Circular dependency in " + expr_
}
