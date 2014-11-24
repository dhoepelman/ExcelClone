package scalaExcel.formula

object ReferenceFinder {

  sealed trait Node
  case class Leaf(e: ACell) extends Node
  case class Leafs(l: List[Leaf]) extends Node
  case class Branch(l: List[Expr]) extends Node
  case object Empty extends Node

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
    case Group(e)             => Branch(List(e))
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
        val rs = List.range(r1, r2 + 1)
        var cs = List.range(colToNum(c1), colToNum(c2) + 1)
        Leafs(for (r <- rs; c <- cs) yield Leaf(ACell(numToCol(c), r)))
      }
    case _ => Empty
  }

  // A => 1, B => 2, AA => 27
  def colToNum(r: String): Int = r.foldLeft(0)(_ * 26 + _ - 64)

  // 1 => A, B => 2, 27 => AA
  def numToCol(r: Int): String =
    if (r >= 1) numToCol((r - 1) / 26) + ((r - 1) % 26 + 65).toChar
    else ""

}
