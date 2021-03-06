package scalaExcel.formula

import scala.util.parsing.combinator._

/**
 * Excel LL grammar based on https://github.com/whatupdave/CilociFormulaEngine/blob/master/Excel.grammar
 */
object Parser extends RegexParsers {
  /**
   * Start parser, this will parse any valid cell value
   **/
  def Start : Parser[Expr] =
    ForceStr          |
    phrase(Formula)   |
    phrase(Primitive) |
    Str               |
    Empty

  //****************************
  //* Literals                 *
  //****************************

  def ForceStr      : Parser[Const]   = "'" ~> """.*""".r ^^ {s => Const(VString(s))}
  // This String is outside of formula, everything that is not anything else is a string
  def Str           : Parser[Const]   = """[^=].*""".r    ^^ {s => Const(VString(s))}
  def Empty         : Parser[Expr]    = ""               ^^^ Const(VEmpty)

  // This string literal can be used inside formula's
  val striReg = """\"(\"\"|[^\"])*\"""".r
  val numbReg = """(\-|\+)?\d+(\.\d+)?([eE]([+-])?\d{1,3})?""".r
  val boolReg = """(?i)(true)|(false)""".r

  def StringLit     : Parser[Const] = striReg ^^ {s => Const(VString(s.substring(1, s.length - 1).replace("\"\"", "\"")))}
  def Num           : Parser[Const] = numbReg ^^ {s => Const(VDouble(s.toDouble))}
  def Bool          : Parser[Const] = boolReg ^^ {s => Const(VBool(s.toBoolean))}

  //****************************
  //* Expressions and formulas *
  //****************************

  def BasicExpression : Parser[Expr]  =
    ExpressionGroup |
    FunctionCall    |
    Reference       |
    Primitive

  def ExpressionGroup   : Parser[Expr]  = "(" ~> Expression <~ ")"  ^^ {e => Group(e)}

  def Primitive : Parser[Expr]  =
    Num              |
    Bool             |
    StringLit        |
    ErrorExpression

  def Formula           : Parser[Expr]  = "=" ~> Expression

  def Expression        : Parser[Expr]  = LogicalExpression

  def FunctionName      : Parser[String]= """(?i)[a-z][\w]*""".r
  def FunctionCall      : Parser[Expr]  = FunctionName ~ "(" ~ Arguments ~ ")" ^^ {
    case f ~ _ ~ args ~ _ => Call(f, args)
  }
  def Arguments         : Parser[List[Expr]] = repsep(Expression, ",")

  def LogicalExpression : Parser[Expr]  = ConcatExpression ~ rep(LogicalOp ~ ConcatExpression) ^^ toBinOp
  def LogicalOp         : Parser[Op2]    =  """=|(>=)|(<=)|(<>)|>|<""".r ^^ {
    case "=" => Eq()
    case ">=" => GTE()
    case "<=" => LTE()
    case "<>" => NEq()
    case ">" => GT()
    case "<" => LT()
  }

  def ConcatExpression  : Parser[Expr]  = AddExpression ~ rep(ConcatOp ~ AddExpression) ^^ toBinOp
  def ConcatOp          : Parser[Op2]    = "&" ^^^ Concat()

  def AddExpression           : Parser[Expr]  = MultExpression ~ rep(AdditiveOp ~ MultExpression ) ^^ toBinOp
  def AdditiveOp[T >: OpAll] : Parser[T]    = """\+|\-""".r ^^  {
    case "+" => Plus()
    case "-" => Minus()
  }

  def MultExpression    : Parser[Expr]  = ExponentExpression ~ rep(MultiplicativeOp ~ ExponentExpression) ^^ toBinOp
  def MultiplicativeOp  : Parser[Op2]    = """\*|\/""".r ^^  {
    case "*" => Mul()
    case "/" => Div()
  }

  def ExponentExpression: Parser[Expr]  = PercentExpression ~ rep(ExponentOp ~ PercentExpression ) ^^ toBinOp
  def ExponentOp        : Parser[Op2]    = "^" ^^^ Expon()

  def PercentExpression : Parser[Expr]  = UnaryExpression ~ rep("%") ^^ {
    case e ~ rest => rest.foldLeft(e) {
      case (l, _) => UnOp(Percent(), l)
    }
  }

  def UnaryExpression   : Parser[Expr] = AdditiveOp[Op1].? ~ BasicExpression ^^ {
    case None ~ e => e
    case Some(op) ~ e => UnOp(op, e)
  }

  // Transforms a Expr [Op Expr [Op Expr [...]]] into a BinOp(Op, Expr, BinOp(Op, Expr, ...))
  def toBinOp(p : Expr ~ List[Op2 ~ Expr]) = p match {
    case e ~ rest => rest.foldLeft(e) {
      case (l, op ~ r) => BinOp(op, l, r)
    }
  }

  def ErrorExpression : Parser[Const]= """(?i)(#[a-z/]+(!|\?)?)""".r^^ { s =>
    Const(ErrType.fromString(s) match {
      case Some(e) => VErr(e)
      case None => VString(s)
    })
  }

  //****************************
  //* References               *
  //****************************

  def Reference : Parser[ParseRef] = SheetName.? ~ GridReference ^^ {
    case None ~ e => e
    case Some(s) ~ e => SheetReference(s, e)
  }

  def GridReference : Parser[ParseRef] =
    CellRange_   |
    RowRange_    |
    ColumnRange_ |
    CellRef_

  def SheetName    : Parser[String] = """(?i)[_a-z][\w]*""".r <~ "!"
  //def DefinedName   : Parser[String]     = """[_a-z][\w]*""".r

  def CellRef_     : Parser[Cell] = ColRef_ ~ RowRef_ ^^ {
    case r ~ c => Cell(r, c)
  }
  def RowRef_      : Parser[RowRef] = "$".? ~ """\d+""".r ^^ {
    // Formula's are one-based, internal references are zero-based
    case a ~ r => RowRef(r.toInt - 1, a.isDefined)
  }
  def ColRef_      : Parser[ColRef] = "$".? ~ """(?i)[a-z]+""".r ^^ {
    case a ~ c => ColRef(colToNum(c), a.isDefined)
  }

  def CellRange_   : Parser[Range]   = CellRef_ ~ ":" ~ CellRef_ ^^ {
    case start ~ _ ~ end => Range(start, end)
  }
  def RowRange_    : Parser[RowRange]= RowRef_ ~ ":" ~ RowRef_ ^^ {
    case r1 ~ _ ~ r2 => RowRange(r1, r2)
  }
  def ColumnRange_ : Parser[ColRange]= ColRef_ ~ ":" ~ ColRef_ ^^ {
    case c1 ~ char ~ c2 => ColRange(c1, c2)
  }

  def parse(s: String) = parseAll(Start, s)

  def parsing(s: String) = parseAll(Start, s).getOrElse(Const(VErr(ParserErr)))

}
