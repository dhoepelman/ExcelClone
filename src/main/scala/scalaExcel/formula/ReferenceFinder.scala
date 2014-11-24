package scalaExcel.formula

import scalaExcel.util.ColumnTranslator

object ReferenceFinder {

  sealed trait Node
  case class Leaf(e: ACell) extends Node
  case class Leafs(l: List[Leaf]) extends Node
  case class Branch(l: List[Expr]) extends Node
  case class Empty() extends Node

  def findRefCells(e: Expr, l: List[ACell] = List()): List[ACell] = getChildren(e) match {
    case Empty     => l
    case Leaf(e)   => l.+:(e)
    case Leafs(ls) => l ++ (ls map ({
      case Leaf(e) => e
    }))
    case Branch(b) => l ++ (b map (findRefCells(_))).flatten
  }

  // get all child nodes
  private def getChildren(e: Expr) = e match {
    case BinOp(_, l, r)       => Branch(List(l, r))
    case UnOp(_, e)           => Branch(List(e))
    case Call(_, args)        => Branch(args)
    case SheetReference(_, e) => Branch(List(e))
    case e: ACell             => Leaf(e)
    case c: Cell              => desugarCell(c)
    case r: Range             => desugarRange(r)
    case _                    => Empty
  }

  private def desugarCell(e: Expr) = e match {
    case Cell(ColRef(c, _), RowRef(r, _)) => Leaf(ACell(c, r))
    case _ => Empty
  }

  private def desugarRange(e: Expr) = e match {
    case Range(
        Cell(ColRef(c1, _), RowRef(r1, _)),
        Cell(ColRef(c2, _), RowRef(r2, _))
      ) => {
        val rs = List.range(r1 - 1, r2)
        val cs = List.range(ColumnTranslator.colToNum(c1) - 1, ColumnTranslator.colToNum(c2))
        Leafs(for (r <- rs; c <- cs) yield Leaf(ACell(ColumnTranslator.numToCol(c), r)))
      }
    case _ => Empty
  }

}
