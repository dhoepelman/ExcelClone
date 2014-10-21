package scalaExcel

import java.awt.Label

import scala.util.parsing.combinator._

/**
 * Excel LL grammar based on https://github.com/whatupdave/CilociFormulaEngine/blob/master/Excel.grammar
 */
class ExcelFormulaParser extends RegexParsers {
  def StringLiteral : Parser[Any] = """\"(\"\"|[^\"]*)\"""".r
  def Number        : Parser[Any] = """\d+(\.\d+)?([e][+-]\d{1,3})?""".r
  def Boolean       : Parser[Any] = """(?i)(true)|(false)""".r

  def Cell          : Parser[Any] = """$?[a-z]{1,2}\$?\d{1,5}""".r
  def CellRange     : Parser[Any] = """\$?[a-z]{1,2}\$?\d{1,5}:\$?[a-z]{1,2}\$?\d{1,5}""".r
  def RowRange      : Parser[Any] = """\$?\d{1,5}:\$?\d{1,5}""".r
  def ColumnRange   : Parser[Any] = """\$?[a-z]{1,2}:\$?[a-z]{1,2}""".r

  def FunctionName  : Parser[Any] = """[a-z][\w]*\(""".r
  def SheetName     : Parser[Any] = """[_a-z][\w]*!""".r
  def DefinedName   : Parser[Any] = """[_a-z][\w]*""".r

  def Formula       : Parser[Any] = "=" ~ Expression

  def Expression    : Parser[Any] = LogicalExpression

  def LogicalExpression = repsep(ConcatExpression, LogicalOp)
  def LogicalOp = "=" | ">" | "<" | ">=" | "<=" | "<>"
  def ConcatExpression = repsep(AdditiveExpression, "&")
  def AdditiveExpression = repsep(MultiplicativeExpression, AdditiveOp)
  def AdditiveOp = "+" | "-"
  def MultiplicativeExpression = repsep(ExponentationExpression, MultiplicativeOp)
  def MultiplicativeOp = "*" | "/"
  def ExponentationExpression = repsep(PercentExpression, "^")
  def PercentExpression = UnaryExpression ~ """%*""".r
  def UnaryExpression = UnaryOp ~ BasicExpression | BasicExpression
  def UnaryOp = "+" | "-"
  def BasicExpression = Primitive | FunctionCall | Reference | ExpressionGroup | ""

  def Reference = DefinedName | GridReferenceExpression
  def GridReferenceExpression = SheetName ~ GridReference | GridReference
  def GridReference = Cell | CellRange | RowRange | ColumnRange

  def FunctionCall = FunctionName ~ Arguments ~ ")"
  def Arguments = repsep(Expression, ",")
  def ExpressionGroup = "(" ~> Expression <~ ")"
  def Primitive = Number | Boolean | StringLiteral | ErrorExpression

  def ErrorExpression = "#DIV/0!" | "#N/A" | "#NAME?" | "#NULL!" | "#REF!" | "#VALUE!" | "#NUM!"
}

object TestParse {
  def main(args: Array[String]) {
    val parser = new ExcelFormulaParser()

    println(parser.parseAll(parser.Expression, "1 * 2 + 3"))
  }
}