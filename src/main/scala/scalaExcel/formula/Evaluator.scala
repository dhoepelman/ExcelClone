
package scalaExcel.formula

import math.{pow, abs}

object Evaluator {

  type Ctx = ACell => Value

  // for most simple operations, no ranges and errors are valid
  def evalIfValidOperand(ctx: Ctx, e: Expr, f: Value => Value) =
    e match {
      case r: ARange => VErr(InvalidValue)
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
        case x :: xs => evalIfValidOperand(ctx, x, v => reduce(ctx, f, f(r, v), xs))
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
    case VEmpty     => VDouble(f(0))
    case _          => VErr(InvalidValue)
  }

  // Apply a function to combine two double values
  def applyToDoubles(f: (Double, Double) => Double)(lhs: Value, rhs: Value): Value = (lhs, rhs) match {
    case (VDouble(l), VDouble(r)) => VDouble(f(l, r))
    case (VBool(b), v)            => applyToDoubles(f)(boolToVDouble(b), v)
    case (v, VBool(b))            => applyToDoubles(f)(v, boolToVDouble(b))
    case (VEmpty, v)              => applyToDoubles(f)(VDouble(0), v)
    case (v, VEmpty)              => applyToDoubles(f)(v, VDouble(0))
    case es => pickError(es, InvalidValue)
  }

  def pickError(t: (Value, Value), default: ErrType) = t match {
    case (e: VErr, _) => e
    case (_, e: VErr) => e
    case _ => VErr(default)
  }

  // Casting of booleans to a double Value doubles
  def boolToVDouble(b: Boolean) = VDouble(if (b) 1.0 else 0.0)

  def boolToString(b: Boolean) = b.toString.toUpperCase

  def doubleToString(d: Double) = if (d.isValidInt) d.toInt.toString else d.toString

  def doubleToVBool(d: Double) = VBool(d != 0)

  def valueToVBool(v: Value) = v match {
    case b: VBool => b
    case VDouble(d) => doubleToVBool(d)
    case VEmpty => VBool(false)
    case x => x
  }

  def valueToVString(v: Value) = v match {
    case VBool(b) => VString(boolToString(b))
    case VDouble(d) => VString(doubleToString(d))
    case VEmpty => VString("")
    case x => x
  }

  // Mainly desugar references

  def desugarArgs(l: List[Expr]): List[Expr] = l match {
    case x :: xs => (desugar(x) match {
      case ARange(cs) => cs
      case y => List(y)
    }) ++ desugarArgs(xs)
    case _ => List()
  }

  def desugar(e: Expr): Expr = e match {
    case c: Cell => desugarCell(c)
    case Range(c1, c2) => desugarRange(c1, c2)
    case Group(e) => desugar(e)
    case _ => e
  }

  def desugarCell(c: Cell) = c match {
    case Cell(ColRef(c, _), RowRef(r, _)) => ACell((c, r))
  }

  def desugarRange(c1: Cell, c2: Cell): Expr =
    (c1, c2) match {
      case (
        Cell(ColRef(c1, _), RowRef(r1, _)),
        Cell(ColRef(c2, _), RowRef(r2, _))
      ) =>
        if (c1 == c2 && r1 == r2)
          ACell((c1, r1))
        else {
          val rs = List.range(r1, r2 + 1)
          val cs = List.range(c1, c2 + 1)
          ARange(for (r <- rs; c <- cs) yield ACell((c,r)))
        }
  }

  // top level eval, returns error for ranges
  def eval(ctx: Ctx, e: Expr) =
    desugar(e) match {
      case ARange(_) => VErr(InvalidValue)
      case x => evalIn_(ctx, x) match {
        case VEmpty => VDouble(0)
        case v => v
      }
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
      case Call(f, args) => evalCall(ctx, f, args)
      case c: ACell => ctx(c)
      case _ => VErr(NA)
    }

  def evalBinOp(ctx: Ctx, op: Op2, lhs: Expr, rhs: Expr) = op match {
    case Eq()     => reduce2(ctx, boolEq, lhs, rhs)
    case GT()     => reduce2(ctx, boolGt, lhs, rhs)
    case LT()     => reduce2(ctx, boolLt, lhs, rhs)
    case GTE()    => reduce2(ctx, boolGte, lhs, rhs)
    case LTE()    => reduce2(ctx, boolLte, lhs, rhs)
    case NEq()    => reduce2(ctx, boolNeq, lhs, rhs)
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

  def concat(lhs: Value, rhs: Value): Value = (valueToVString(lhs), valueToVString(rhs)) match {
    case (VString(s1), VString(s2)) => VString(s1 + s2)
    case es => pickError(es, NA)
  }

  private def emptyToZero(v: Value) = v match {
    case VEmpty => VDouble(0)
    case _ => v
  }

  private def boolCmp[T](cmp: (Int => T))(lhs: Value, rhs: Value) =
    cmp(emptyToZero(lhs).compare(emptyToZero(rhs)))

  def boolEq  = boolCmp(c => VBool(c == 0)) _
  def boolNeq = boolCmp(c => VBool(c != 0)) _
  def boolGt  = boolCmp(c => VBool(c > 0)) _
  def boolGte = boolCmp(c => VBool(c >= 0)) _
  def boolLt  = boolCmp(c => VBool(c < 0)) _
  def boolLte = boolCmp(c => VBool(c <= 0)) _

  def doubleDiv(lhs: Value, rhs: Value) = applyToDoubles(_ / _)(lhs, rhs) match {
    case VDouble(d) =>  if (d.isInfinity) VErr(DivBy0)
                        else VDouble(d)
    case x => x
  }

  def doubleExpon(base: Value, expon: Value): Value = (base, expon) match {
    case (VDouble(0), VDouble(0)) => VErr(NotNumeric)
    case (b, e) => applyToDoubles(pow)(base, expon) match {
      case VDouble(d) => if (d.isInfinity) VErr(DivBy0)
                         else if (d.isNaN) VErr(NotNumeric)
                         else VDouble(d)
      case x => x
    }
    case _ => VErr(InvalidValue)
  }

  def evalCall(ctx: Ctx, fn: String, args: List[Expr]) =
    fn match {
      case "SUM"     => reduce(ctx, applyToDoubles(_ + _), VDouble(0), desugarArgs(args))
      case "AVERAGE" => evalCallAverage(ctx, desugarArgs(args))
      case "ROWS"    => evalCallRows(args)
      case "COLUMNS" => evalCallColumns(args)
      case "COUNT"   => evalCallCount(ctx, desugarArgs(args))
      case "IF"      => evalCallIf(ctx, args)
      case "OR"      => evalCallOr(ctx, desugarArgs(args))
      case "AND"     => evalCallAnd(ctx, desugarArgs(args))
      case "NOT"     => evalCallNot(ctx, args)
      case "POWER"   => evalCallPower(ctx, args)
      case "UPPER"   => evalCallUpper(ctx, args)
      case "LOWER"   => evalCallLower(ctx, args)
      case "LEN"     => evalCallLen(ctx, args)
      case "TRIM"    => evalCallTrim(ctx, args)
      case _ => VErr(InvalidName)
    }

  def evalCallAverage(ctx: Ctx, args: List[Expr]) =
    evalIn(ctx, BinOp(Div(), Call("SUM", args), Const(VDouble(args.length))))

  def evalCallRows(args: List[Expr]) = args match {
    case List(Range(Cell(_, RowRef(r1, _)), Cell(_, RowRef(r2, _)))) => {
      VDouble(abs(r2 - r1) + 1)
    }
    case _ => throw new Exception("Wrong number of arguments")
  }

  def evalCallColumns(args: List[Expr]) = args match {
    case List(Range(Cell(ColRef(c1, _), _), Cell(ColRef(c2, _), _))) => {
      VDouble(abs(c2 - c1) + 1)
    }
    case _ => throw new Exception("Wrong number of arguments")
  }

  def evalCallCount(ctx: Ctx, args: List[Expr]) =
    VDouble(args
      .map(arg => evalIn(ctx, arg) match {
        case VDouble(_) => 1
        case _ => 0
      })
      .fold(0)(_+_))

  def evalCallIf(ctx: Ctx, args: List[Expr]) = (args match {
    // Normalize length of arguments to 3
    case List(condition) => List(condition, Const(VBool(true)), Const(VBool(false)))
    case List(condition, trueValue) => List(condition, trueValue, Const(VBool(false)))
    case x => x
  }) match {
    // evaluate the condition and then either the true or left value
    case List(condition, trueValue, falseValue) => valueToVBool(evalIn(ctx, condition)) match {
      case VBool(b) => evalIn(ctx, if (b) trueValue else falseValue)
      case _ => VErr(InvalidValue)
    }
    case _ => throw new Exception("Wrong number of arguments")
  }

  def evalCallOr(ctx: Ctx, args: List[Expr]) = {
    args.foldLeft[Value](VBool(false))((res, arg) => res match {
      case VBool(true) => res
      case _ => valueToVBool(evalIn(ctx, arg))
    })
  }

  def evalCallAnd(ctx: Ctx, args: List[Expr]) = {
    args.foldLeft[Value](VBool(true))((res, arg) => res match {
      case VBool(false) => res
      case _ => valueToVBool(evalIn(ctx, arg))
    })
  }

  def evalCallNot(ctx: Ctx, args: List[Expr]) = args match {
    case List(arg) => valueToVBool(evalIn(ctx, arg)) match {
      case VBool(v) => VBool(!v)
      case _ => VErr(InvalidValue)
    }
    case _ => throw new Exception("Wrong number of arguments")
  }

  def evalCallPower(ctx: Ctx, args: List[Expr]) = args match {
    case List(num, exp) => evalIn(ctx, BinOp(Expon(), num, exp))
    case _ => throw new Exception("Wrong number of arguments")
  }

  def evalWithString(f: String => Value)(ctx: Ctx, args: List[Expr]) = {
    args match {
      case List(value) => valueToVString(evalIn(ctx, value)) match {
        case VString(str) => f(str)
        case x => x
      }
      case _ => throw new Exception("Wrong number of arguments")
    }
  }

  def evalCallUpper = evalWithString(str => VString(str.toUpperCase)) _

  def evalCallLower = evalWithString(str => VString(str.toLowerCase)) _

  def evalCallLen = evalWithString(str => VDouble(str.length)) _

  def evalCallTrim = evalWithString(str => VString(str.trim)) _

}
