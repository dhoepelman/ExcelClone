package scalaExcel.model

import scalaExcel.formula.{Evaluator, ACell, Value, Parser, VDouble}
import scalaExcel.formula.ReferenceFinder.findRefCells
import scalaExcel.util.ColumnTranslator.{numToCol, colToNum}

// This is a cell object, it can execute it self and find the references inside
// the formula. This implements a dummy parser/executor
class Cell(
    val x: Int,
    val y: Int,
    val f: String
  ) {

  // AST of the cell
  lazy val AST = Cell.parser.parsing(f)

  // Dependencies of this cell
  lazy val refs: List[(Int, Int)] = findRefCells(AST).map(Cell.ACellToPos)

  val position = (x, y)

  // Get the current value of this cell
  def eval(deps: Map[(Int, Int), Value]): Value = Evaluator.eval(Ctx(deps), AST)

  private def Ctx(values: Map[(Int, Int), Value])(c: ACell) = values get ((colToNum(c.c) - 1, c.r - 1)) match {
    case Some(v) => v
    case None => VDouble(0)//throw new IllegalArgumentException(s"Dependency (${c.c},${c.r}}) not found in map")
  }

}

object Cell {
  val parser = new Parser()

  def posToACell(c: Int, r: Int) = ACell(numToCol(c + 1), r + 1)
  def ACellToPos(c: ACell) = (colToNum(c.c) - 1, c.r - 1)
}
