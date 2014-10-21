
package scalaExcel

import scala.util.parsing.combinator._

class Parser extends JavaTokenParsers {
  def expr: Parser[Any] = term~rep("+"~term | "-"~term)
  def term: Parser[Any] = factor~rep("*"~factor | "/"~factor)
  def factor: Parser[Any] = boolean | floatingPointNumber | "("~expr~")"

  def boolean: Parser[Term] = ("true" | "false") ^^ (s =>
      TermBool(s.toBoolean))

}

