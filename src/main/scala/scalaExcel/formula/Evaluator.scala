
package scalaExcel.formula

import math.pow

object Evaluator {

  type Ctx = ACell => Value

  // for most simple operations, no ranges and errors are valid
  def evalIfValidOperand(ctx: Ctx, e: Expr, f: Value => Value) =
    e match {
      case r: ARange => VErr(InvalidValue())
      case _ => evalIn(ctx, e) match {
        case err: VErr => err
        case v => f(v)
      }
    }

  // reduce a list of expressions to a single value
  def reduce(ctx: Ctx, f: ((Value, Value) => Value), r: Value, args: List[Expr]): Value =
    r match {
      case e: VErr => e
      case _ => args match {
        case x :: xs => evalIfValidOperand(ctx, x, (v => reduce(ctx, f, f(r, v), xs)))
        case _ => r
      }
    }

  // combine two expressions to a single value
  def reduce2(ctx: Ctx, f: ((Value, Value) => Value), lhs: Expr, rhs: Expr) =
    evalIfValidOperand(ctx, lhs, l => reduce(ctx, f, l, List(rhs)))

  // Apply a function to one double value
  def applyToDouble(f: (Double => Double))(v: Value): Value = v match {
    case VDouble(d) => VDouble(f(d))
    case VBool(b)   => applyToDouble(f)(boolToVDouble(b))
    case _          => VErr(InvalidValue())
  }

  // Apply a function to combine two double values
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

  // Casting of booleans to a double Value doubles
  def boolToVDouble(b: Boolean) = VDouble(if (b) 1.0 else 0.0)

  def boolToString(b: Boolean) = b.toString.toUpperCase

  def doubleToString(d: Double) = if (d.ceil == d) d.toInt.toString else d.toString

  // Mainly desugar references

  def desugarArgs(l: List[Expr]): List[Expr] = l match {
    case x :: xs => (desugar(x) match {
      case ARange(cs) => cs
      case y => List(y)
    }) ++ desugarArgs(xs)
    case _ => List()
  }

  def desugar(e: Expr) = e match {
    case c: Cell => desugarCell(c)
    case Range(c1, c2) => desugarRange(c1, c2)
    case _ => e
  }
  def desugarCell(c: Cell) = c match {
    case Cell(ColRef(c, _), RowRef(r, _)) => ACell(c, r)
  }

  def desugarRange(c1: Cell, c2: Cell): Expr =
    if (c1 != c2)
      ARange(List(ACell("A", 1), ACell("A", 2)))
    else
      c1 match {
        case Cell(ColRef(c1, _), RowRef(r1, _)) => ACell(c1, r1)
      }

  // top level eval, returns error for ranges
  def eval(ctx: Ctx, e: Expr) =
    desugar(e) match {
      case ARange(_) => VErr(InvalidValue())
      case x => evalIn_(ctx, x)
    }

  // internal eval with desugaring
  def evalIn(ctx: Ctx, e: Expr): Value =
    evalIn_(ctx, desugar(e))

  // internal eval without desugaring
  def evalIn_(ctx: Ctx, e: Expr): Value =
    e match {
      case Const(c) => c
      case BinOp(op, lhs, rhs) => evalBinOp(ctx, op, desugar(lhs), desugar(rhs))
      case UnOp(op, v) => evalUnOp(ctx, op, desugar(v))
      case Call(f, args) => evalCall(ctx, f, desugarArgs(args))
      case c: ACell => ctx(c)
      case _ => VErr(NA())
    }

  def evalBinOp(ctx: Ctx, op: Op2, lhs: Expr, rhs: Expr) = op match {
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
  }

  def evalUnOp(ctx: Ctx, op: Op1, v: Expr) = op match {
    // Unary Plus is a no-op in Excel
    case Plus()    => eval(ctx, v)
    case Minus()   => applyToDouble(- _)(eval(ctx, v))
    case Percent() => applyToDouble(_ / 100)(eval(ctx, v))
  }

  def concat(lhs: Value, rhs: Value): Value = (lhs, rhs) match {
    case (v, VDouble(d)) => concat(v, VString(doubleToString(d)))
    case (VDouble(d), v) => concat(VString(doubleToString(d)), v)
    case (v, VBool(b))   => concat(v, VString(boolToString(b)))
    case (VBool(b), v)   => concat(VString(boolToString(b)), v)
    case (VString(s1), VString(s2)) => VString(s1 + s2)
    case es => pickError(es, NA())
  }

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

  def evalCall(ctx: Ctx, f: String, args: List[Expr]) =
    f match {
      case "SUM" => reduce(ctx, applyToDoubles(_ + _), VDouble(0), args)
      case _ => VErr(InvalidName())
    }

  def evalCell(ctx: Ctx, col: ColRef, row: RowRef) = (col, row) match {
    case (ColRef(c, _), RowRef(r, _)) => eval(ctx, ACell(c, r))
  }

}
