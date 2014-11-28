package scalaExcel.model

import scalaExcel.formula.{Evaluator, ACell, Value, Parser, VDouble}
import scalaExcel.formula.ReferenceFinder.findRefCells
import scalaExcel.util.ColumnTranslator.{numToCol, colToNum}

// This is a cell object, it can execute it self and find the references inside
// the formula. This implements a dummy parser/executor
sealed trait Cell {

  /** Cell position */
  def position : CellPos

  /** Cell AST */
  lazy val AST : Expr = Cell.parser.parsing(f)

  /** Cell formula */
  lazy val f : String = PPrinter.pprint(AST)

  /** Dependencies of this cell */
  lazy val refs: List[(Int, Int)] = ReferenceFinder.findRefCells(AST).map(Cell.ACellToPos)

  /** Get the current value of this cell */
  def eval(deps: Map[(Int, Int), Value]): Value = Evaluator.eval(Ctx(deps), AST)

  override def toString = '"' + f + '"'

  protected def Ctx(values: Map[(Int, Int), Value])(c: ACell) = values get ((colToNum(c.c), c.r - 1)) match {
    case Some(v) => v
    case None => VDouble(0)//throw new IllegalArgumentException(s"Dependency (${c.c},${c.r}}) not found in map")
  }

}

object Cell {
  def apply(pos : CellPos) : Cell = new EmptyCell(pos)
  def apply(x : Int, y : Int, f : String) : Cell = new FormulaCell((x,y), f)
  def apply(pos : CellPos, f : String) : Cell = new FormulaCell(pos, f)
  def apply(pos : CellPos, AST : Expr) : Cell = new ASTCell(pos, AST)
  def apply(pos : CellPos, original : Cell) : Cell = new CopyCell(pos, original.AST, original.f, original.refs)

  val parser = new Parser()

  def posToACell(c: Int, r: Int) = ACell(numToCol(c), r + 1)
  def ACellToPos(c: ACell) = (colToNum(c.c), c.r - 1)

  private class EmptyCell(val position : CellPos) extends Cell {
    override lazy val f = ""
    override lazy val AST = Const(VString(""))
    override lazy val refs = List()
  }
  private class ASTCell(val position : CellPos, val AST_ : Expr) extends Cell {
    override lazy val AST = AST_
  }
  private class FormulaCell(val position : CellPos, val f_ : String) extends Cell {
    override lazy val f = f_
  }
  private class CopyCell(val position : CellPos, val AST_ : Expr, val f_ : String, val refs_ : List[CellPos]) extends Cell {
    override lazy val f = f_
    override lazy val AST = AST_
    override lazy val refs = refs_
  }
}
