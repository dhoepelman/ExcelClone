package scalaExcel.formula

sealed abstract class Expr

case class Const(v: Value) extends Expr

case class Cell(c: ColRef, r: RowRef) extends Expr
case class RowRef(referent: Int, absolute: Boolean)
case class ColRef(referent: String, absolute: Boolean)

case class Range(start: Cell, end: Cell) extends Expr
case class RowRange(start: RowRef, end: RowRef) extends Expr
case class ColRange(start: ColRef, end: ColRef) extends Expr

case class SheetReference(sheet: String, e: Expr) extends Expr

case class Call(f: String, args: List[Expr]) extends Expr

case class BinOp(op: Op, e1: Expr, e2: Expr) extends Expr
case class UnOp(op: Op, e: Expr) extends Expr

sealed abstract class Op
case class Eq()     extends Op
case class GT()     extends Op
case class LT()     extends Op
case class GTE()    extends Op
case class LTE()    extends Op
case class NEq()    extends Op

case class Concat() extends Op
case class Plus()   extends Op
case class Minus()  extends Op
case class Mul()    extends Op
case class Div()    extends Op
case class Expon()  extends Op
case class Percent() extends Op

case class Err(t: ErrType) extends Expr
