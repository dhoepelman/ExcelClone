package scalaExcel.model

import scalaExcel.formula.{ACell, Value, Parser, VDouble}

// This is a cell object, it can execute it self and find the references inside
// the formula. This implements a dummy parser/executor
class Cell(val x: Int, val y: Int, val f: String) {
  lazy val AST = Cell.parser.parsing(f)

  /*
   val sumInt = """=(\d+)\+(\d+)""".r
   val sumRef = """=([A-Z]+)\+([A-Z]+)""".r

   // either the formula is adding two numbers
   val (lhs, rhs) = sumInt findFirstIn f match {
     case Some(sumInt(lhs, rhs)) => (lhs.toInt, rhs.toInt)
     case None => (0, 0)
   }

   // or it has two references that are added
   val refs = ((sumRef findFirstIn f) match {
     case Some(sumRef(lhs, rhs)) => List(lhs, rhs)
     case _ => List()
   }) map (_ match {
     case "A" => (1, 1)
     case "B" => (2, 1)
     case x => throw new Exception(s"Don't know $x?!")
   })
   */

  def refs : List[(Int,Int)] = ???

  val position = (x, y)

   // Add all numbers and references
   def eval(deps: Map[(Int, Int), Value]) : Value = Cell.evaluator.eval(Cell.Ctx(deps), AST)

}

object Cell {
  val parser = new Parser()
  val evaluator = scalaExcel.formula.Evaluator

  def Ctx(values: Map[(Int, Int), Value])(c : ACell) = values get (colToNum(c.c), c.r) match {
    case Some(v) => v
    case None    => throw new IllegalArgumentException(s"Cell (${c.c},${c.r}}) not found in Ctx")
  }

  // A => 1, B => 2, AA => 27
  def colToNum(r: String): Int = r.foldLeft(0)(_ * 26 + _ - 64)

  // 1 => A, B => 2, 27 => AA
  def numToCol(r: Int): String =
    if (r >= 1) numToCol((r - 1) / 26) + ((r - 1) % 26 + 65).toChar
    else ""
}
