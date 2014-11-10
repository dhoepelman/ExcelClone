
package scalaExcel.formula

import math.pow

object Evaluator {

  type Ctx = Map[ACell, Value]

  def reduce(ctx: Ctx, f: ((Value, Value) => Value), r: Value, args: List[Expr]): Value = r match {
    case e: VErr => e
    case _ => args match {
      case x :: xs => eval(ctx, x) match {
        case e: VErr => e
        case i => reduce(ctx, f, f(r, i), xs)
      }
      case _ => r
    }
  }

  def reduce2(ctx: Ctx, f: ((Value, Value) => Value), lhs: Expr, rhs: Expr) = eval(ctx, lhs) match {
    case e: VErr => e
    case l => eval(ctx, rhs) match {
      case e: VErr => e
      case r => f(l, r)
    }
  }

  def applyToDouble(f: (Double => Double))(v: Value): Value = v match {
    case VDouble(d) => VDouble(f(d))
    case VBool(b)   => applyToDouble(f)(boolToVDouble(b))
    case e: VErr    => e
    case _          => VErr(InvalidValue())
  }

  def applyToDoubles(f: (Double, Double) => Double)(lhs: Value, rhs: Value): Value = (lhs, rhs) match {
    case (VDouble(l), VDouble(r)) => VDouble(f(l, r))
    case (VBool(b), v)            => applyToDoubles(f)(boolToVDouble(b), v)
    case (v, VBool(b))            => applyToDoubles(f)(v, boolToVDouble(b))
    case es => pickError(es, InvalidValue())
  }

  def pickError(t: (Value, Value), default: ErrType) = t match {
    case (e: VErr, _) => e
    case (_, e: VErr) => e
    case _ => VErr(default)
  }

  def boolToVDouble(b: Boolean) = VDouble(if (b) 1.0 else 0.0)

  def eval(ctx: Ctx, e: Expr): Value = {
    e match {
      case Const(c) => c
      case BinOp(op, lhs, rhs) => evalBinOp(ctx, op, lhs, rhs)
      case UnOp(op, v) => evalUnOp(ctx, op, v)
      case Call(f, args) => evalCall(ctx, f, args)
      case c: ACell => evalACell(ctx, c)
      case Cell(c, r) => evalCell(ctx, c, r)
      case _ => VErr(NA())
    }
  }

  def evalBinOp(ctx: Ctx, op: Op, lhs: Expr, rhs: Expr) = op match {
    case Eq()     => reduce2(ctx, boolEq, lhs, rhs)
    case GT()     => reduce2(ctx, boolGt, lhs, rhs)
    case LT()     => reduce2(ctx, boolLt, lhs, rhs)
    case GTE()    => reduce2(ctx, boolGte, lhs, rhs)
    case LTE()    => reduce2(ctx, boolLte, lhs, rhs)
    case NEq()    => reduce2(ctx, boolNe, lhs, rhs)
    case Concat() => reduce2(ctx, concat, lhs, rhs)
    case Plus()   => reduce2(ctx, applyToDoubles(_ + _), lhs, rhs)
    case Minus()  => reduce2(ctx, applyToDoubles(_ - _), lhs, rhs)
    case Mul()    => reduce2(ctx, applyToDoubles(_ * _), lhs, rhs)
    case Div()    => reduce2(ctx, doubleDiv, lhs, rhs)
    case Expon()  => reduce2(ctx, doubleExpon, lhs, rhs)
    case _ => throw new IllegalArgumentException("Invalid BinOp in AST")
  }

  def evalUnOp(ctx: Ctx, op: Op, v: Expr) = op match {
    // Unary Plus is a no-op in Excel
    case Plus()    => eval(ctx, v)
    case Minus()   => applyToDouble(- _)(eval(ctx, v))
    case Percent() => applyToDouble(_ / 100)(eval(ctx, v))
    case _ => throw new IllegalArgumentException("Invalid UnOp in AST")
  }

  def concat(lhs: Value, rhs: Value): Value = (lhs, rhs) match {
    case (v, VDouble(d)) => concat(v, VString(doubleToString(d)))
    case (VDouble(d), v) => concat(VString(doubleToString(d)), v)
    case (v, VBool(b))   => concat(v, VString(b.toString.toUpperCase))
    case (VBool(b), v)   => concat(VString(b.toString.toUpperCase), v)
    case (VString(s1), VString(s2)) => VString(s1 + s2)
    case es => pickError(es, NA())
  }

  def doubleToString(d: Double) =
    if (d.ceil == d) d.toInt.toString
    else d.toString

  def boolEq(lhs: Value, rhs: Value) = (lhs, rhs) match {
    case (VBool(l), VBool(r))     => VBool(l == r)
    case (VDouble(l), VDouble(r)) => VBool(l == r)
    case (VString(l), VString(r)) => VBool(l == r)
    case _ => VBool(false)
  }

  def boolGt(lhs: Value, rhs: Value) = (lhs, rhs) match {
    case (VBool(l), VBool(r))     => VBool(l > r)
    case (VDouble(l), VDouble(r)) => VBool(l > r)
    case (VString(l), VString(r)) => VBool(l > r)
    case (VBool(l), VDouble(r))   => VBool(true)
    case _ => VBool(false)
  }

  def boolLt(lhs: Value, rhs: Value) = (lhs, rhs) match {
    case (VBool(l), VBool(r))     => VBool(l < r)
    case (VDouble(l), VDouble(r)) => VBool(l < r)
    case (VString(l), VString(r)) => VBool(l < r)
    case (VDouble(l), VBool(r))   => VBool(true)
    case _ => VBool(false)
  }

  def boolGte(lhs: Value, rhs: Value) = boolGt(lhs, rhs) match {
    case VBool(true) => VBool(true)
    case _ => boolEq(lhs, rhs)
  }

  def boolLte(lhs: Value, rhs: Value) = boolLt(lhs, rhs) match {
    case VBool(true) => VBool(true)
    case _ => boolEq(lhs, rhs)
  }

  def boolNe(lhs: Value, rhs: Value) = boolEq(lhs, rhs) match {
    case VBool(b) => VBool(!b)
  }

  def doubleDiv(lhs: Value, rhs: Value) = applyToDoubles(_ / _)(lhs, rhs) match {
    case VDouble(d) =>  if (d.isInfinity) VErr(DivBy0())
                        else VDouble(d)
    case x => x
  }

  def doubleExpon(base: Value, expon: Value): Value = (base, expon) match {
    case (VDouble(0), VDouble(0)) => VErr(NotNumeric())
    case (b, e) => applyToDoubles(pow)(base, expon) match {
      case VDouble(d) => if (d.isInfinity) VErr(DivBy0())
                         else if (d.isNaN) VErr(NotNumeric())
                         else VDouble(d)
      case x => x
    }
    case _ => VErr(InvalidValue())
  }

  def evalCall(ctx: Ctx, f: String, args: List[Expr]) = {
    f match {
      case "SUM" => reduce(ctx, applyToDoubles(_ + _), VDouble(0), args)
      case _ => VErr(InvalidName())
    }
  }

  def evalACell(ctx: Ctx, c: ACell) = {
    ctx.get(c) match {
      case Some(v) => v
      case None    => VErr(NA())
    }
  }

  def evalCell(ctx: Ctx, col: ColRef, row: RowRef) = (col, row) match {
    case (ColRef(c, _), RowRef(r, _)) => eval(ctx, ACell(c, r))
  }

}
