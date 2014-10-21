package scalaExcel

import java.awt.Label

import scala.util.parsing.combinator._


class ExcelFormulaParser extends RegexParsers {
  def expression    : Parser[Any] = term ~ rep(addop ~ term)
  def term          : Parser[Any] = factor ~ rep(mulop ~ factor)
  def factor        : Parser[Any] = number | "(" ~> expression <~ ")" | cellRef | function

  def function      : Parser[Any] = functionName ~ "(" ~> functionArgs <~ ")"
  def functionArgs  : Parser[Any] = "" | expression ~ rep("," ~ expression)

  def Formula       : Parser[Any] = "=" ~ Expression
  def Expression    : Parser[Any] = Primitive | Cell | Function | Label

  def ReferencePrefix : Parser[Any] = Sheet ~ Reference | Reference

  def Reference     : Parser[Any] = Cell

  def Sheet         : Parser[Any] = Sheetname ~ "!"
  def Sheetname     : Parser[Any] = """[a-z0-9]+""".r
}
