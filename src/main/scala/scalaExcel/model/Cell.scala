package scalaExcel.model

import scalaExcel.formula.{ACell, Value, Parser, VDouble}

// This is a cell object, it can execute it self and find the references inside
// the formula. This implements a dummy parser/executor
class Cell(val x: Int, val y: Int, val f: String) {
  /**
   * AST of the cell
   */
  lazy val AST = Cell.parser.parsing(f)
  /**
   * Dependencies of this cell
   */
  lazy val refs: List[(Int, Int)] = Cell.referenceFinder.findRefCells(AST).map(Cell.ACellToPos)

  val position = (x, y)

  /**
   * Add all numbers and references
   */
  def eval(deps: Map[(Int, Int), Value]): Value = Cell.evaluator.eval(Ctx(deps), AST)

  private def Ctx(values: Map[(Int, Int), Value])(c: ACell) = values get(Cell.colToNum(c.c), c.r) match {
    case Some(v) => v
    case None => throw new IllegalArgumentException(s"Dependency (${c.c},${c.r}}) not found in map")
  }
}

object Cell {
  val parser = new Parser()
  val evaluator = scalaExcel.formula.Evaluator
  val referenceFinder = scalaExcel.formula.ReferenceFinder

  // A => 1, B => 2, AA => 27
  def colToNum(r: String): Int = r.foldLeft(0)(_ * 26 + _ - 64)

  // 1 => A, B => 2, 27 => AA
  def numToCol(r: Int): String =
    if (r >= 1) numToCol((r - 1) / 26) + ((r - 1) % 26 + 65).toChar
    else ""

  def posToACell(c: Int, r: Int) = ACell(numToCol(c), r)
  def ACellToPos(c: ACell) = (colToNum(c.c), c.r)
}
