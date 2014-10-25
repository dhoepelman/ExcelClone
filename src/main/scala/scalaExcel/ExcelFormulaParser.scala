package scalaExcel

import scala.util.parsing.combinator._

/**
 * Excel LL grammar based on https://github.com/whatupdave/CilociFormulaEngine/blob/master/Excel.grammar
 */
class ExcelFormulaParser extends RegexParsers {
  def Start         : Parser[Expr] = Formula | Bool | Num | Str | Empty

  // This String is outside of formula, everything that is not anything else is a string
  def Str           : Parser[Const]   =  """[^=].*""".r                       ^^ { s => Const(s)}
  def Empty         : Parser[Expr]    = ""                                    ^^^ Const("")

  // This string literal can be used inside formula's
  def StringLiteral : Parser[Const]   = """\"(\"\"|[^\"]*)\"""".r             ^^ { s => Const(s.replace("\"\"", "\""))}
  def Num           : Parser[Const]   = """\d+(\.\d+)?([e][+-]\d{1,3})?""".r  ^^ { s => Const(s.toDouble)}
  def Bool          : Parser[Const]   = """(?i)(true)|(false)""".r            ^^ { s => Const(s.toBoolean)}

  // TODO: Generalize the repearted functions of this style. Not trivial due to types
  //def toNestedBinOps(left : Expr, right : List[String ~ Expr]) : Parser[Expr] = {right.foldLeft(left) { case (l, op ~ r) => BinOp(op, l,r)} }

  // TODO: In
  def Expression        : Parser[Expr]= LogicalExpression

  def LogicalExpression : Parser[Expr]= ConcatExpression ~ rep(LogicalOp ~ ConcatExpression) ^^ {case e ~ rest => rest.foldLeft(e) { case (l, op ~ r) => BinOp(op, l,r)} }
  def LogicalOp         : Parser[Op]  =  """=|(>=)|(<=)|(<>)|>|<""".r ^^ {case "=" => Eq() case ">=" => GTE() case "<=" => LTE() case "<>" => NEq() case ">" => GT() case "<" => LT()}

  def ConcatExpression  : Parser[Expr]= AddExpression ~ rep("&" ~ AddExpression) ^^ {case e ~ rest => rest.foldLeft(e) { case (l, _ ~ r) => BinOp(Concat(), l,r)} }

  def AddExpression     : Parser[Expr]= MultExpression ~ rep(AdditiveOp ~ MultExpression ) ^^ {case e ~ rest => rest.foldLeft(e) { case (l, op ~ r) => BinOp(op, l,r)} }
  def AdditiveOp        : Parser[Op]  = """\+|\-""".r ^^  {case "+" => Plus() case "-" => Minus()}

  def MultExpression    : Parser[Expr]= ExponentExpression ~ rep(AdditiveOp ~ ExponentExpression) ^^ {case e ~ rest => rest.foldLeft(e) { case (l, op ~ r) => BinOp(op,l,r)} }
  def MultiplicativeOp  : Parser[Op]  = """\*|\/""".r ^^  {case "*" => Mul() case "/" => Div()}

  def ExponentExpression: Parser[Expr]= PercentExpression ~ rep("^" ~ PercentExpression ) ^^ {case e ~ rest => rest.foldLeft(e) { case (l, _ ~ r) => BinOp(Expon(), l,r)} }

  def PercentExpression : Parser[Expr]= UnaryExpression ~ rep("%") ^^ {case e ~ rest => rest.foldLeft(e) { case (l, _) => UnOp(Percent(), l)}}

  def UnaryExpression   : Parser[Expr]= AdditiveOp.? ~ BasicExpression ^^ {case None ~ e => e case Some(op) ~ e => UnOp(op, e)}

  def Formula           : Parser[Expr]= "=" ~> Expression

  def FunctionName      : Parser[String]= """[a-z][\w]*""".r
  def FunctionCall      : Parser[Expr]= FunctionName ~ "(" ~ Arguments ~ ")" ^^ {case f ~ _ ~ args ~ _ => Call(f, args)}
  def Arguments         : Parser[List[Expr]] = repsep(Expression, ",")
  def ExpressionGroup   : Parser[Expr]= "(" ~> Expression <~ ")"
  def Primitive = Num | Bool | StringLiteral | ErrorExpression

  def ErrorExpression   : Parser[Err]= """(?i)(#DIV/0!)|(#N/A)|(#NAME?)|(#NUM!)|(#NULL!)|(#REF!)|(#VALUE!)""".r^^ {case "#DIV/0!" => Err(DivBy0())
  case "#N/A" => Err(NA())
  case "#NAME?" => Err(InvalidName())
  case "#NUM!" => Err(NotNumeric())
  case "#NULL!" => Err(Null())
  case "#REF!" => Err(InvalidRef())
  case "#VALUE!" => Err(InvalidValue())}

  def BasicExpression   : Parser[Expr] = Primitive | FunctionCall | Reference | ExpressionGroup

  def Reference         : Parser[Expr] = SheetName.? ~ GridReference ^^ {case None ~ e => e case Some(s) ~ e => SheetReference(s, e)}
  def GridReference = Cellref | Cellrange | Rowrange | Columnrange

  def SheetName     : Parser[String]     = """[_a-z][\w]*!""".r
  //def DefinedName   : Parser[String]     = """[_a-z][\w]*""".r

  def Cellref       : Parser[Cell]    = Colref ~ Rowref              ^^ { case r ~ c => Cell(r, c)}
  def Rowref        : Parser[RowRef]  = "$".? ~ """\d+""".r          ^^ { case a ~ r => RowRef(r.toInt, a.isDefined)}
  def Colref        : Parser[ColRef]  = "$".? ~ """(?i)[a-z]+""".r   ^^ { case a ~ c => ColRef(c, a.isDefined)}
  // TODO: Remove the ":" from the transformation by using <~ or ~>
  def Cellrange     : Parser[Range]   = Cellref ~ ":" ~ Cellref      ^^ { case start ~ _ ~ end => Range(start, end)}
  def Rowrange      : Parser[RowRange]= Rowref ~ ":" ~ Rowref        ^^ { case r1 ~ _ ~ r2 => RowRange(r1, r2)}
  def Columnrange   : Parser[ColRange]= Colref ~ ":" ~ Colref        ^^ { case c1 ~ char ~ c2 => ColRange(c1, c2)}


}

object TestParse {
  def main(args: Array[String]) {
    val parser = new ExcelFormulaParser()

    println(parser.parseAll(parser.Expression, "1 ^ 5"))
  }
}