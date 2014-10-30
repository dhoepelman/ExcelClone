
package scalaExcel.formula

object Evaluator {

  def eval(e: Expr): Value = {
    e match {
      case Const(c) => c
      case Call(f, args) => evalCall(f, args)
      case BinOp(op, lhs, rhs) => evalBinOp(op, lhs, rhs)
      case _ => VErr(NA())
    }
  }

  def evalBinOp(op: Op, lhs: Expr, rhs: Expr) = op match {
    case Plus() => reduce(sum, VDouble(0), List(lhs, rhs))
    case _ => VErr(NA())
  }

  def evalCall(f: String, args: List[Expr]) = {
    f match {
      case "SUM" => reduce(sum, VDouble(0), args)
      case _ => VErr(NA())
    }
  }

  def reduce(f: ((Value, Value) => Value), r: Value, args: List[Expr]): Value = r match {
    case e: VErr => e
    case _ => args match {
      case x :: xs => eval(x) match {
        case e: VErr => e
        case i => reduce(f, f(i, r), xs)
      }
      case _ => r
    }
  }

  def sum(lhs: Value, rhs: Value) = lhs match {
    case VDouble(l) => rhs match {
      case VDouble(r) => VDouble(l + r)
      case _ => VErr(NotNumeric())
    }
    case _ => VErr(NotNumeric())
  }

}
