package scalaExcel.formula

import scalaExcel.util.ColumnTranslator.{colToNum, numToCol}

object ReferenceFinder {

  sealed trait Node
  case class Leaf(e: ACell) extends Node
  case class Leafs(l: List[Leaf]) extends Node
  case class Branch(l: List[Expr]) extends Node
  case object Empty extends Node

  def findRefCells(e: Expr, l: List[ACell] = List()): List[ACell] = getChildren(e) match {
    case Empty     => l
    case Leaf(e2)   => l.+:(e2)
    case Leafs(ls) => l ++ (ls map {
      case Leaf(e2) => e2
    })
    case Branch(b) => l ++ (b map (findRefCells(_))).flatten
  }

  // get all child nodes
  private def getChildren(e: Expr) = e match {
    case BinOp(_, l, r)       => Branch(List(l, r))
    case UnOp(_, e2)           => Branch(List(e2))
    case Call(_, args)        => Branch(args)
    case SheetReference(_, e2) => Branch(List(e2))
    case Group(e2)             => Branch(List(e2))
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
      ) =>
        val rs = List.range(r1, r2 + 1)
        val cs = List.range(c1, c2 + 1)
        Leafs(for (r <- rs; c <- cs) yield Leaf(ACell((c, r))))
    case _ => Empty
  }

}
