package scalaExcel

import scala.util.parsing.combinator._

/**
 * Excel LL grammar based on https://github.com/whatupdave/CilociFormulaEngine/blob/master/Excel.grammar
 */
class ExcelFormulaParser extends RegexParsers {
  /**
   * Start parser, this will parse any valid cell value
   **/
  def Start : Parser[Expr] =
    Formula   |
    Primitive |
    Empty

  //****************************
  //* Literals                 *
  //****************************

  // This String is outside of formula, everything that is not anything else is a string
  def Str           : Parser[Const]   = """[^=].*""".r ^^ {s => Const(s)}
  def Empty         : Parser[Expr]    = ""             ^^^ Const("")

  // This string literal can be used inside formula's
  val striReg = """\"(\"\"|[^\"]*)\"""".r
  val numbReg = """\d+(\.\d+)?([eE]([+-])?\d{1,3})?""".r
  val boolReg = """(?i)(true)|(false)""".r

  def StringLit     : Parser[Const]  = striReg ^^ {s => Const(s.replace("\"\"", "\"").substring(1, s.length - 1))}
  def PosNum        : Parser[Double] = numbReg ^^ {s => s.toDouble}
  def Num           : Parser[Const]  = AdditiveOp.? ~ PosNum ^^ {
    case None ~ e => Const(e)
    case Some(op) ~ e => op match {
      case Minus() => Const(-e)
      case Plus()  => Const(e)
      case _       => throw new IllegalStateException() // This should never happen
    }
  }
  def Bool          : Parser[Const] = boolReg ^^ {s => Const(s.toBoolean)}

  //****************************
  //* Expressions and formulas *
  //****************************

  def BasicExpression   : Parser[Expr]  =
    Primitive       |
    FunctionCall    |
    Reference       |
    ExpressionGroup

  def ExpressionGroup   : Parser[Expr]  = "(" ~> Expression <~ ")"
  def Primitive         : Parser[Expr]  =
    Num              |
    Bool             |
    StringLit        |
    ErrorExpression

  def Formula           : Parser[Expr]  = "=" ~> Expression

  def FunctionName      : Parser[String]= """[a-z][\w]*""".r
  def FunctionCall      : Parser[Expr]  = FunctionName ~ "(" ~ Arguments ~ ")" ^^ {
    case f ~ _ ~ args ~ _ => Call(f, args)
  }
  def Arguments         : Parser[List[Expr]] = repsep(Expression, ",")

  def Expression        : Parser[Expr]  = LogicalExpression

  def LogicalExpression : Parser[Expr]  = ConcatExpression ~ rep(LogicalOp ~ ConcatExpression) ^^ toBinOp
  def LogicalOp         : Parser[Op]    =  """=|(>=)|(<=)|(<>)|>|<""".r ^^ {
    case "=" => Eq()
    case ">=" => GTE()
    case "<=" => LTE()
    case "<>" => NEq()
    case ">" => GT()
    case "<" => LT()
  }

  def ConcatExpression  : Parser[Expr]  = AddExpression ~ rep(ConcatOp ~ AddExpression) ^^ toBinOp
  def ConcatOp          : Parser[Op]    = "&" ^^^ Concat()

  def AddExpression     : Parser[Expr]  = MultExpression ~ rep(AdditiveOp ~ MultExpression ) ^^ toBinOp
  def AdditiveOp        : Parser[Op]    = """\+|\-""".r ^^  {
    case "+" => Plus()
    case "-" => Minus()
  }

  def MultExpression    : Parser[Expr]  = ExponentExpression ~ rep(MultiplicativeOp ~ ExponentExpression) ^^ toBinOp
  def MultiplicativeOp  : Parser[Op]    = """\*|\/""".r ^^  {
    case "*" => Mul()
    case "/" => Div()
  }

  def ExponentExpression: Parser[Expr]  = PercentExpression ~ rep(ExponentOp ~ PercentExpression ) ^^ toBinOp
  def ExponentOp        : Parser[Op]    = "^" ^^^ Expon()

  def PercentExpression : Parser[Expr]  = UnaryExpression ~ rep("%") ^^ {
    case e ~ rest => rest.foldLeft(e) {
      case (l, _) => UnOp(Percent(), l)
    }
  }

  def UnaryExpression   : Parser[Expr] = AdditiveOp.? ~ BasicExpression ^^ {
    case None ~ e => e
    case Some(op) ~ e => UnOp(op, e)
  }

  // Transforms a Expr [Op Expr [Op Expr [...]]] into a BinOp(Op, Expr, BinOp(Op, Expr, ...))
  def toBinOp(p : Expr ~ List[Op ~ Expr]) = p match {
    case e ~ rest => rest.foldLeft(e) {
      case (l, op ~ r) => BinOp(op, l, r)
    }
  }

  def ErrorExpression   : Parser[Err]= """(?i)(#DIV/0!)|(#N/A)|(#NAME?)|(#NUM!)|(#NULL!)|(#REF!)|(#VALUE!)""".r^^ {
    case "#DIV/0!"  => Err(DivBy0())
    case "#N/A"     => Err(NA())
    case "#NAME?" => Err(InvalidName())
    case "#NUM!" => Err(NotNumeric())
    case "#NULL!" => Err(Null())
    case "#REF!" => Err(InvalidRef())
    case "#VALUE!" => Err(InvalidValue())
  }

  //****************************
  //* References               *
  //****************************

  def Reference     : Parser[Expr] = SheetName.? ~ GridReference ^^ {
    case None ~ e => e
    case Some(s) ~ e => SheetReference(s, e)
  }
  def GridReference =
    Cellref     |
    Cellrange   |
    Rowrange    |
    Columnrange

  def SheetName     : Parser[String] = """[_a-z][\w]*!""".r
  //def DefinedName   : Parser[String]     = """[_a-z][\w]*""".r

  def Cellref       : Parser[Cell] = Colref ~ Rowref ^^ {
    case r ~ c => Cell(r, c)
  }
  def Rowref        : Parser[RowRef] = "$".? ~ """\d+""".r ^^ {
    case a ~ r => RowRef(r.toInt, a.isDefined)
  }
  def Colref        : Parser[ColRef] = "$".? ~ """(?i)[a-z]+""".r ^^ {
    case a ~ c => ColRef(c.toUpperCase, a.isDefined)
  }

  // TODO: Remove the ":" from the transformation by using <~ or ~>
  def Cellrange     : Parser[Range]   = Cellref ~ ":" ~ Cellref ^^ {
    case start ~ _ ~ end => Range(start, end)
  }
  def Rowrange      : Parser[RowRange]= Rowref ~ ":" ~ Rowref ^^ {
    case r1 ~ _ ~ r2 => RowRange(r1, r2)
  }
  def Columnrange   : Parser[ColRange]= Colref ~ ":" ~ Colref ^^ {
    case c1 ~ char ~ c2 => ColRange(c1, c2)
  }

  def parsing(s: String) = parseAll(Start, s) match {
    case Success (t, _) => t
    case NoSuccess(msg, _) => throw new IllegalArgumentException(s"Unable to parse $s: $msg")
  }

}
// TODO: Remove this once the parser is debugged and unit tests are in place
object TestParse {
  def main(args: Array[String]) {
    val parser = new ExcelFormulaParser()
    println(parser.parseAll(parser.Start, "=1 * 5"))
  }
}
