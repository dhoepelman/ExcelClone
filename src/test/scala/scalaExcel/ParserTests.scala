
package scalaExcel

import org.junit.Test
import org.junit.Assert._

class ParserTests {

  val parser = new Parser()

  def parse(s: String) = parser.parseAll(parser.expr, s)

  @Test def testIntLiteral = assertEquals(
    TermNum(4),
    parse("4"))

  @Test def testDoubleLiteral = assertEquals(
    TermNum(2.5),
    parse("2.5"))

  @Test def testBoolLiteralTrue = assertEquals(
    TermBool(true),
    parse("true"))

  @Test def testBoolLiteralFalse = assertEquals(
    TermBool(false),
    parse("false"))

  @Test def testPlusExpr = assertEquals(
      TermAdd(TermNum(1), TermNum(2)),
      parse("1 + 2"))

  @Test def testProdTerm = assertEquals(
      TermMult(TermAdd(TermNum(1), TermNum(2)), TermNum(3)),
      parse("(1 + 2) * 3"))

}
