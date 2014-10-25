
package scalaExcel

import junit.framework.TestCase
import org.junit._
import org.junit.Assert._

import scala.util.parsing.input.CharSequenceReader

class ParserTests extends ExcelFormulaParser {
  implicit val testee = Start

  private def parsing[T](s : String)(implicit p : Parser[T]) =
    // phrase ensures that all input is consumed
    phrase(p)(new CharSequenceReader(s)) match {
      case Success (t,_) => t
      case NoSuccess(msg, _) => throw new IllegalArgumentException(s"Unable to parse $s: $msg")
    }

  private def assertFail[T](input : String)(implicit p : Parser[T]) = {
    try {
      parsing(input)(p)
      fail(s"Parsed $input which should've failed")
    } catch {
      case e : IllegalArgumentException =>
      case e : Exception => fail(e.getMessage)
    }
  }

  @Test def intLiteral= List(-100, -1, 0, 1, 1025).foreach( i => {
    assertEquals(Const(i.toDouble), parsing(i.toString)) ;
    assertEquals(Const(i.toDouble), parsing("=" + i.toString))
  })
  @Test def floatLiteral = List(-500.0, -1.0, 0.0, 1.0, 1.0/3.0, Math.PI).foreach( i => {
    assertEquals(Const(i), parsing(i.toString)) ;
    assertEquals(Const(i), parsing("=" + i.toString))
  })
  @Test def scientificNotationLiteral = List("-1e3", "1.23E+10").foreach( i => {
    assertEquals(Const(i.toDouble), parsing(i)) ;
    assertEquals(Const(i.toDouble), parsing("=" + i))
  })

  @Test def booleanLiteral = {
    assertEquals(Const(true), parsing("true"))
    assertEquals(Const(true), parsing("TRUE"))
    assertEquals(Const(true), parsing("truE"))
    assertEquals(Const(false), parsing("false"))
    assertEquals(Const(false), parsing("FALSE"))
    assertEquals(Const(false), parsing("FAlSe"))
    assertEquals(Const(true), parsing("=true"))
    assertEquals(Const(false), parsing("=false"))
    assertEquals(Const(true), parsing("=TRUE"))
  }

  @Test def concat1 = assertEquals(BinOp(Concat(), Const(1), Const(1)), parsing("=1 & 1"))
  @Test def concat2 = assertEquals(BinOp(Concat(), Const("a"), Const("b")), parsing("=\"a\" & \"b\""))
  @Test def concat3 = assertEquals(BinOp(Concat(), BinOp(Concat(), Const("a"), Const("b")), Const("c")), parsing("=\"a\" & \"b\" & \"c\""))
  @Test def concat4 = assertEquals(BinOp(Concat(),BinOp(Concat(),BinOp(Concat(),Const("a"),Const("b")),Const("c")),Const("d")), parsing("=\"a\" & \"b\" & \"c\" & \"d\""))
  @Test def concat5 = assertEquals(BinOp(Concat(), Const("a"), BinOp(Concat(), Const("b"), BinOp(Concat(), Const("c"), Const("d")))), parsing("=\"a\" & (\"b\" & (\"c\" & \"d\"))"))
  @Test def concat6 = assertEquals(parsing("=\"a\" & \"b\" & \"c\" & \"d\""), parsing("=(((\"a\" & \"b\") & \"c\") & \"d\")"))

  // The following formula's are from http://ewbi.blogs.com/develops/2004/12/excel_formula_p.html
  @Test def complex1 {parsing("=IF(\"a\"={\"a\",\"b\";\"c\",#N/A;-1,TRUE}, \"yes\", \"no\") &   \"  more \"\"test\"\" text\"")}
  @Test def complex2 {parsing("=IF(R13C3>DATE(2002,1,6),0,IF(ISERROR(R[41]C[2]),0,IF(R13C3>=R[41]C[2],0, IF(AND(R[23]C[11]>=55,R[24]C[11]>=20),R53C3,0))))")}
  @Test def complex3 {parsing("=IF(R[39]C[11]>65,R[25]C[42],ROUND((R[11]C[11]*IF(OR(AND(R[39]C[11]>=55, R[40]C[11]>=20),AND(R[40]C[11]>=20,R11C3=\"YES\")),R[44]C[11],R[43]C[11]))+(R[14]C[11] *IF(OR(AND(R[39]C[11]>=55,R[40]C[11]>=20),AND(R[40]C[11]>=20,R11C3=\"YES\")), R[45]C[11],R[43]C[11])),0))")}

  // The following formula's are from http://homepages.mcs.vuw.ac.nz/~elvis/db/Excel.shtml
  @Test def example01 {parsing("=1")}
  @Test def example02 {parsing("=1+1")}
  @Test def example03 {parsing("=A1")}
  @Test def example04 {parsing("=$B$2")}
  @Test def example05 {parsing("=SUM(B5:B15)")}
  @Test def example06 {parsing("=SUM(B5:B15,D5:D15)")}
  @Test def example07 {parsing("=SUM(B5:B15 A7:D7)")}
  @Test def example08 {parsing("=SUM(sheet1!$A$1:$B$2)")}
  @Test def example09 {parsing("=[data.xls]sheet1!$A$1")}
  @Test def example10 {parsing("=SUM((A:A 1:1))")}
  @Test def example11 {parsing("=SUM((A:A A1:B1))")}
  @Test def example12 {parsing("=SUM((D9:D11,(E9:E11,F9:F11)))")}
  @Test def example13 {parsing("=IF(P5=1.0,\"NA\",IF(P5=2.0,\"A\",IF(P5=3.0,\"B\",IF(P5=4.0,\"C\",IF(P5=5.0,\"D\",IF(P5=6.0,\"E\",IF(P5=7.0,\"F\",IF(P5=8.0,\"G\"))))))))")}
  // Enable if Array formula's are implemented
  //@Test def example14 {parsing("={SUM(B2:D2*B3:D3)}")}

/*
  @Test def testBoolLiteralTrue = assertEquals(
    TermBool(true),
    parse("true"))

  @Test def testBoolLiteralFalse = assertEquals(
    TermBool(false),
    parse("false"))

  @Test def testPlusExpr = assertEquals(
      TermAdd(TermNum(1), TermNum(2)),
      parse("=1 + 2"))

  @Test def testProdTerm = assertEquals(
      TermMult(TermAdd(TermNum(1), TermNum(2)), TermNum(3)),
      parse("=(1 + 2) * 3"))
*/
}
