
package scalaExcel.formula

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

  def applyToDouble(f: (Double => Double))(v: Value) = v match {
    case e: VErr => e
    case VDouble(d) => VDouble(f(d))
    case _ => VErr(NotNumeric())
  }

  def applyToDoubles(f: (Double, Double) => Double)(lhs: Value, rhs: Value) = (lhs, rhs) match {
    case (VDouble(l), VDouble(r)) => VDouble(f(l, r))
    case es => pickError(es, NotNumeric())
  }

  def pickError(t: (Value, Value), default: ErrType) = t match {
    case (e: VErr, _) => e
    case (_, e: VErr) => e
    case _ => VErr(default)
  }

  def eval(e: Expr): Value = {
    e match {
      case Const(c) => c
      case Call(f, args) => evalCall(f, args)
      case BinOp(op, lhs, rhs) => evalBinOp(op, lhs, rhs)
      case UnOp(op, v) => evalUnOp(op, v)
      case _ => VErr(NA())
    }
  }

  def evalCall(f: String, args: List[Expr]) = {
    f match {
      case "SUM" => reduce(applyToDoubles(_ + _), VDouble(0), args)
      case _ => VErr(NA())
    }
  }

  def evalBinOp(op: Op, lhs: Expr, rhs: Expr) = op match {
    case Eq()     => reduce2(boolEq, lhs, rhs)
    case GT()     => reduce2(boolGt, lhs, rhs)
    case LT()     => reduce2(boolLt, lhs, rhs)
    case GTE()    => VBool(false)
    case LTE()    => VBool(false)
    case NEq()    => VBool(false)
    case Concat() => reduce(concat, VString(""), List(lhs, rhs))
    case Plus()   => reduce(applyToDoubles(_ + _), VDouble(0), List(lhs, rhs))
    case Minus()  => VDouble(0)
    case Mul()    => reduce(applyToDoubles(_ * _), VDouble(1), List(lhs, rhs))
    case Div()    => VDouble(0)
    case Expon()  => VDouble(0)
    case _ => VErr(NA())
  }

  def evalUnOp(op: Op, v: Expr) = op match {
    case Plus()    => applyToDouble(+ _)(eval(v))
    case Minus()   => applyToDouble(- _)(eval(v))
    case Percent() => applyToDouble(_ / 100)(eval(v))
    case _ => VErr(NA())
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
    case _ => VBool(false)
  }

  def boolLt(lhs: Value, rhs: Value) = (lhs, rhs) match {
    case (VBool(l), VBool(r))     => VBool(l < r)
    case (VDouble(l), VDouble(r)) => VBool(l < r)
    case (VString(l), VString(r)) => VBool(l < r)
    case (VDouble(l), VBool(r))   => VBool(true)
    case _ => VBool(false)
  }

}
