package scalaExcel.formula

/**
 * Pretty prints the AST
 */
object PPrinter {

  /** Pretty print an AST  */
  def pprint(AST: Expr): String = AST match {
    // Test if a string parses to itself, if not force that by prepending '
    case Const(VString(s)) => Parser parsing s match {
      case Const(VString(_)) => s
      case _ => "'" + s
    }
    case Const(c) => print(c)
    case x => "=" + print(x)
  }

  private def print(expr: Expr): String = expr match {
    case Const(c) => print(c)
    case BinOp(op, e1, e2) => print(e1) + " " + print(op) + " " + print(e2)
    case UnOp(op, e) => op match {
      case Percent() => print(e) + print(op)
      case _ => print(op) + print(e)
    }
    case Call(f, args) => f + "(" + args.tail.foldLeft(print(args.head))((acc,arg) => acc + "," + print(arg)) + ")"
    case Group(e) => "(" + print(e) + ")"
    case Cell(c, r) => print(c) + print(r)
    case ACell(pos) => "$" + numToCol(pos._1) + "$" + (pos._2+1)
    case ARange(l) => print(l)
    case Range(start, end) => print(start) + ":" + print(end)
    case RowRange(start, end) => print(start) + ":" + print(end)
    case ColRange(start, end) => print(start) + ":" + print(end)
    case SheetReference(sheet, ref) => sheet + "!" + print(ref)
    case _ => throw new IllegalArgumentException
  }

  private def print(ref: RowRef) = $(ref.absolute) + (ref.referent+1)
  private def print(ref: ColRef) = $(ref.absolute) + numToCol(ref.referent)
  private def $(b: Boolean) = if (b) "$" else ""

  private def print(op: Op) = op match {
    case Eq() => "=="
    case GT() => ">"
    case LT() => "<"
    case GTE() => ">="
    case LTE() => "<="
    case NEq() => "<>"
    case Concat() => "&"
    case Plus() => "+"
    case Minus() => "-"
    case Mul() => "*"
    case Div() => "/"
    case Expon() => "^"
    case Percent() => "%"
  }

  private def print(v: Value) = v match {
    case VBool(b) => b.toString
    case VDouble(d) => if (d.isValidInt) d.toInt.toString else d.toString
    case VString(s) => "\"" + s + "\""
    case VErr(t) => t.expr
    case VEmpty => ""
  }

  private def _getACellRow(cell: ACell) = cell.pos._2
  private def _getACellCol(cell: ACell) = cell.pos._1

  private def print(l: List[ACell]) = {
    val maxCol = _getACellCol(l.maxBy(_getACellCol))
    val minCol = _getACellCol(l.minBy(_getACellCol))
    val maxRow = _getACellRow(l.maxBy(_getACellRow))
    val minRow = _getACellRow(l.minBy(_getACellRow))
    "$" + minCol + "$" + minRow + ":$" + maxCol + "$" + maxRow
  }

}
