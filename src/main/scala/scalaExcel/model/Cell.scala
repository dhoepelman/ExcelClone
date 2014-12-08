package scalaExcel.model

import scalaExcel.CellPos
import scalaExcel.formula._

// This is a cell object, it can execute it self and find the references inside
// the formula. This implements a dummy parser/executor
sealed trait Cell {

  /** Cell AST */
  lazy val AST : Expr = Parser parsing(f)

  /** Cell formula */
  lazy val f : String = PPrinter.pprint(AST)

  /** Dependencies of this cell */
  lazy val refs: List[CellPos] = ReferenceFinder.findRefCells(AST).map( _.pos )

  /** Get the current value of this cell */
  def eval(deps: Map[CellPos, Value]): Value = Evaluator.eval(Ctx(deps), AST)

  override def toString = '"' + f + '"'

  protected def Ctx(values: Map[CellPos, Value])(c: ACell) = values.getOrElse(c.pos, VEmpty)

}

object Cell {
  def apply() : Cell = EmptyCell
  def apply(f : String) : Cell = new FormulaCell(f)
  def apply(AST : Expr) : Cell = new ASTCell(AST)

  private object EmptyCell extends Cell {
    override lazy val f = ""
    override lazy val AST = Const(VEmpty)
    override lazy val refs = List()
  }
  private class ASTCell(val AST_ : Expr) extends Cell {
    override lazy val AST = AST_
  }
  private class FormulaCell(val f_ : String) extends Cell {
    override lazy val f = f_
  }
}
