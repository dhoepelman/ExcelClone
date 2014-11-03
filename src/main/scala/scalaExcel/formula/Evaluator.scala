
package scalaExcel.formula

import math.pow

object Evaluator {

  def reduce(f: ((Value, Value) => Value), r: Value, args: List[Expr]): Value = r match {
    case e: VErr => e
    case _ => args match {
      case x :: xs => eval(x) match {
        case e: VErr => e
        case i => reduce(f, f(r, i), xs)
      }
      case _ => r
    }
  }

  def reduce2(f: ((Value, Value) => Value), lhs: Expr, rhs: Expr) = eval(lhs) match {
    case e: VErr => e
    case l => eval(rhs) match {
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

  def eval(e: Expr): Value = {
    e match {
      case Const(c) => c
      case BinOp(op, lhs, rhs) => evalBinOp(op, lhs, rhs)
      case UnOp(op, v) => evalUnOp(op, v)
      case Call(f, args) => evalCall(f, args)
      case _ => VErr(NA())
    }
  }

  def evalBinOp(op: Op, lhs: Expr, rhs: Expr) = op match {
    case Eq()     => reduce2(boolEq, lhs, rhs)
    case GT()     => reduce2(boolGt, lhs, rhs)
    case LT()     => reduce2(boolLt, lhs, rhs)
    case GTE()    => reduce2(boolGte, lhs, rhs)
    case LTE()    => reduce2(boolLte, lhs, rhs)
    case NEq()    => reduce2(boolNe, lhs, rhs)
    case Concat() => reduce2(concat, lhs, rhs)
    case Plus()   => reduce2(applyToDoubles(_ + _), lhs, rhs)
    case Minus()  => reduce2(applyToDoubles(_ - _), lhs, rhs)
    case Mul()    => reduce2(applyToDoubles(_ * _), lhs, rhs)
    case Div()    => reduce2(doubleDiv, lhs, rhs)
    case Expon()  => reduce2(doubleExpon, lhs, rhs)
    case _ => VErr(NA())
  }

  def evalUnOp(op: Op, v: Expr) = op match {
    case Plus()    => unOpPlus(eval(v))
    case Minus()   => applyToDouble(- _)(eval(v))
    case Percent() => applyToDouble(_ / 100)(eval(v))
    case _ => VErr(NA())
  }

  def unOpPlus(v: Value) = v match {
    case VString(v) => VString(v)
    case v          => applyToDouble(+ _)(v)
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

  def evalCall(f: String, args: List[Expr]) = {
    f match {
      case "SUM" => reduce(applyToDoubles(_ + _), VDouble(0), args)
      case _ => VErr(InvalidName())
    }
  }

}