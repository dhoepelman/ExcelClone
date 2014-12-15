package scalaExcel.formula

import scalaExcel.CellPos

/** Modifies cell dependencies in their AST */
object DependencyModifier {

  /**
   * Change every dependency of ''from'' to ''to''
   * Only single dependencies, not in ranges
   *
   * Use this when cut-pasting
   *
   * @example (=A1, (0,0), (1,2)) becomes =B3
   * @example (=SUM(A1:A5), (0,0), (1,2)) stays =SUM(A1:A5)
   */
  def changeDependency(from: CellPos, to: CellPos): (Expr) => Expr =
    changeDependency((pos: CellPos) => if(pos == from) to else pos)

  private def cellToPos(c: Cell): CellPos = c match {
    case Cell(ColRef(c2, _), RowRef(r, _)) => (c2, r)
  }

  private def posToCell(p : CellPos, absc : Boolean, absr : Boolean) =
    Cell(ColRef(p._1, absc), RowRef(p._2, absr))

  private def changeCellPos(c: Cell, newPos: CellPos) = c match {
    case Cell(ColRef(_, absc), RowRef(_, absr)) => posToCell(newPos, absc, absr)
  }

  /**
   * Move the non-absolute dependencies of a cell relative to its new position
   * Use this when copy-pasting
   * @example (=B1+C1, (2,0), (3,2)) becomes =C3+D3
   * @example (=$B1+C$1), (2,0), (3,2)) becomes =$B3+D$1
   */
  def moveDependencies(from: CellPos, to: CellPos): (Expr => Expr) = {

    val dX = to._1 - from._1
    val dY = to._2 - from._2

    def moveCell(c: Cell) = c match {
      case Cell(c, r) => Cell(moveC(c), moveR(r))
    }
    def moveC(c: ColRef) = c match {
      case ColRef(c2, abs) => if (abs) c else ColRef(c2 + dX, abs)
    }
    def moveR(r: RowRef) = r match {
      case RowRef(r2, abs) => if (abs) r else RowRef(r2 + dY, abs)
    }

    applyToAST(e => e match {
      case c: Cell => checkValidRef(moveCell(c))
      case Range(start, end) => checkValidRef(Range(moveCell(start), moveCell(end)))
      case RowRange(r1, r2) => checkValidRef(RowRange(moveR(r1), moveR(r2)))
      case ColRange(c1, c2) => checkValidRef(ColRange(moveC(c1), moveC(c2)))
      case _: ACell => throw new IllegalArgumentException
      case _: ARange => throw new IllegalArgumentException
      case _ => e
    })
  }

  private def checkValidRef(e : ParseRef) : Expr = {
    val error = Const(VErr(InvalidRef))
    e match {
      case Cell(ColRef(c,_), RowRef(r, _)) => if(c < 0 || r < 0) error else e
      case Range(start, end) => if(checkValidRef(start) == error || checkValidRef(end) == error) error else e
      case RowRange(RowRef(r1,_), RowRef(r2,_)) => if(r1 < 0 || r2 < 0) error else e
      case ColRange(ColRef(c1, _), ColRef(c2, _)) => if(c1 < 0 || c2 < 0) error else e
      case SheetReference(f, e2) => if(checkValidRef(e2) == error) error else e
    }
  }

  /** Apply a function to the whole AST */
  private def applyToAST(f: Expr => Expr)(e: Expr) = e match {
    case BinOp(op, e1, e2) => BinOp(op, f(e1), f(e2))
    case UnOp(op, e) => UnOp(op, f(e))
    case Call(n, args) => Call(n, args map f)
    case Group(e) => Group(f(e))
    case SheetReference(s, r) => f(r) match {
      case r2 : ParseRef => checkValidRef(SheetReference(s,r2))
      /** TODO: It would be better to return SheetReference(s,Err) like Excel does
       * However that requires some refactoring because Const is not a ParseRef.
       * So either let SheetReference accept any Expr or make a case object InvalidRef extend ParseRef
       * and make sure that gets proccesed correctly and used everywhere instead of Const(VErr(InvalidRef)).
       * On evaluation InvalidRef should then turn into VErr(InvalidRef)
       */
      case Const(VErr(InvalidRef)) => Const(VErr(InvalidRef))
      case _ => throw new IllegalArgumentException("AST transformation did not return a valid ParseRef for SheetReference")
    }
    case _ => f(e)
  }

  /**
   * Change the dependencies of formulas by applying a given repositioning
   * function on each cell reference
   * @param repositioner  repositioning function of type (CellPos) => CellPos
   * @return              the AST modifier
   *
   * Use this as a general dependency modifier for single dependencies,
   * not in ranges
   * @example ((A1, A3), ((0, 0) => (2, 4), (0, 2) => (1, 3))) results into (C5, B4)
   */
  def changeDependency(repositioner: (CellPos) => CellPos) = applyToAST(e => e match {
    case c: Cell =>
      changeCellPos(c, repositioner(cellToPos(c)))
    // Easily possible, but probably indicates a mistake elsewhere
    case inv: ACell => throw new IllegalArgumentException("Invalid AST type")
    case _ => e
  }) _

}
