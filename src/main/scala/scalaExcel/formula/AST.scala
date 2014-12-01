package scalaExcel.formula

import scalaExcel.model.CellPos

sealed trait Expr

case class Const(v: Value) extends Expr

// entirely desugared, atomic, cell reference
sealed trait AbsRef extends Expr
case class ACell(pos : CellPos) extends AbsRef
case class ARange(l: List[ACell]) extends AbsRef

// raw cell references
sealed trait ParseRef extends Expr
case class Cell(c: ColRef, r: RowRef) extends Expr with ParseRef
case class RowRef(referent: Int, absolute: Boolean)
case class ColRef(referent: Int, absolute: Boolean)
case class Range(start: Cell, end: Cell) extends Expr with ParseRef
case class RowRange(start: RowRef, end: RowRef) extends Expr with ParseRef
case class ColRange(start: ColRef, end: ColRef) extends Expr with ParseRef
case class SheetReference(sheet: String, e: ParseRef) extends Expr with ParseRef

case class Call(f: String, args: List[Expr]) extends Expr

case class BinOp(op: Op2, e1: Expr, e2: Expr) extends Expr
case class UnOp(op: Op1, e: Expr) extends Expr

/** Expression group, enclosed in parentheses. Ignored in calculation but used for pretty-printing */
case class Group(e : Expr) extends Expr

sealed trait Op
sealed trait Op1 extends Op
sealed trait Op2 extends Op
sealed trait OpAll extends Op with Op1 with Op2

case class Eq()      extends Op2
case class GT()      extends Op2
case class LT()      extends Op2
case class GTE()     extends Op2
case class LTE()     extends Op2
case class NEq()     extends Op2
case class Concat()  extends Op2
case class Plus()    extends OpAll
case class Minus()   extends OpAll
case class Mul()     extends Op2
case class Div()     extends Op2
case class Expon()   extends Op2
case class Percent() extends Op1
