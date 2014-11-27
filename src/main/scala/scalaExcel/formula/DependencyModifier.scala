package scalaExcel.formula

import scalaExcel.model.CellPos

/** Modifies cell dependencies in their AST */
object DependencyModifier {
  /**
   * Change every dependency of ''from'' to ''to''
   * Only single dependencies, not in ranges
   *
   * @example (=A1, (1,1), (2,3)) becomes =B3
   * @example (=SUM(A1:A5), (1,1), (2,3)) stays =SUM(A1:A5)
   */
  def changeDependency(AST: Expr, from: CellPos, to: CellPos): Expr = {
    changeDependency_(from, to)(AST)
  }

  private def changeDependency_(from: CellPos, to: CellPos)(e: Expr): Expr = {
    val me : Expr => Expr = changeDependency_(from, to)
    e match {
      case c: Cell => if (cellToPos(c) == from) changeCellPos(c, to) else e
      // Easily possible, but probably indicates a mistake elsewhere
      case inv: ACell => throw new IllegalArgumentException("Invalid AST type")
      // TODO: Maybe there's an easier/better way to "fix" these parameters?
      case _ => applyToAST(me, e)
    }
  }

  private def cellToPos(c: Cell): CellPos = c match {
    case Cell(ColRef(c, _), RowRef(r, _)) => (colToNum(c), r)
  }

  private def changeCellPos(c: Cell, newPos: CellPos) = c match {
    case Cell(ColRef(_, absc), RowRef(_, absr)) => Cell(ColRef(numToCol(newPos._1), absc), RowRef(newPos._2, absr))
  }

  /**
   * Move the non-absolute dependencies of a cell relative to its new position
   * @example (=B1+C1, (3,1), (4,3)) becomes =C3+D3
   * @example (=$B1+C$1), (3,1), (4,3)) becomes =$B3+D$1
   */
  def moveDependencies(AST: Expr, from: CellPos, to: CellPos): Expr = {
    val dX = to._1 - from._1
    val dY = to._2 - from._2
    moveDependencies_(dX, dY)(AST)
  }

  private def moveDependencies_(dX: Int, dY: Int)(e: Expr): Expr = {
    val me : Expr => Expr = moveDependencies_(dX, dY)

    def moveCell(c : Cell) = c match { case Cell(c, r) => Cell(moveC(c), moveR(r)) }
    def moveC(c : ColRef) = c match { case ColRef(c2, abs) => if (abs) c else ColRef(numToCol(colToNum(c2) + dX), abs) }
    def moveR(r : RowRef) = r match { case RowRef(r2, abs) => if (abs) r else RowRef(r2 + dY, abs) }

    e match {
      case c: Cell => moveCell(c)
      case Range(start, end) => Range(moveCell(start), moveCell(end))
      case RowRange(r1, r2) => RowRange(moveR(r1), moveR(r2))
      case ColRange(c1, c2) => ColRange(moveC(c1), moveC(c2))
      case _ : ACell => throw new IllegalArgumentException
      case _ : ARange => throw new IllegalArgumentException
      case _ => applyToAST(me, e)
    }
  }

  // TODO: Is this useful for other classes too?
  // TODO: Is there a canonical way/name for this operation? I feel like I'm re-inventing the wheel
  /** Apply a function to the whole AST */
  private def applyToAST(f: Expr => Expr, e: Expr) = e match {
    case BinOp(op, e1, e2) => BinOp(op, f(e1), f(e2))
    case UnOp(op, e) => UnOp(op, f(e))
    case Call(n, args) => Call(n, args map f)
    case Group(e) => Group(f(e))
    case SheetReference(s, r) => f(r) match {
      case r2 : ParseRef => SheetReference(s,r2)
      case _ => throw new IllegalArgumentException("AST transformation did not return a valid ParseRef for SheetReference")
    }
    case _ => e
  }
}
