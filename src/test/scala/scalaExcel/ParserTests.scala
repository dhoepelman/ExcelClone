
package scalaExcel

import junit.framework.TestCase
import org.junit._
import org.junit.Assert._

class ParserTests {

  val p = new ExcelFormulaParser()

  private def assertFail(input: String) = {
    try {
      p parsing(input)
      fail(s"Parsed $input which should've failed")
    } catch {
      case e : IllegalArgumentException =>
      case e : Exception => fail(e.getMessage)
    }
  }

  val consts = List(100, 2.5, -10, "foo", true)

  @Test def doubleConstEquaility = assertTrue(Const(10.0) equals Const(10))

  @Test def constEquality = consts foreach
    (i => assertTrue(Const(i) equals Const(i)))

  @Test def constAssertEquals = consts foreach
    (i => assertEquals(Const(i), Const(i)))

  @Test def t1 = p test(Const(100), "100")

  @Test def intLiteral = List(0, 1, 1025) foreach (i => {
    p test(Const(i.toDouble), i.toDouble.toString)
    p test(Const(i.toDouble), "=" + i.toString)
  })

  @Test def floatLiteral = List(500.0, 1.0, 0.0, 1.0, 1.0 / 3.0, Math.PI).foreach(i => {
    p test(Const(i), i.toString) ;
    p test(Const(i), "=" + i.toString)
  })

  @Test def unOpNegate = List(0, 1, 1024, 1.0 / 3.0) foreach (i => {
    p test(Const(-i), "-" + i)
  })

  @Test def scientificNotationLiteral =
    List("1e3", "1.23E+10", "12e5", "1E+02", "8.7E-3")
      .foreach(i => {
        p test(Const(i.toDouble), i)
        p test(Const(i.toDouble), "=" + i)
        p test(Const(-i.toDouble), "-" + i)
      })

  @Test def invalidScientificNotationLiteral =
    List("1.0e0.3", "1e1234", "1e-1234")
      .foreach (i => assertFail(i))

  @Test def booleanLiterals =
    Map(
      "true" -> true,
      "TRUE" -> true,
      "truE" -> true,
      "false" -> false,
      "FALSE" -> false,
      "FaLSE" -> false
    ) foreach (kv => {
      p test(Const(kv._2), kv._1)
      p test(Const(kv._2), "=" + kv._1)
    })

/*
  @Test def concat1 = assertEquals(BinOp(Concat(), Const(1), Const(1)), parsing("=1 & 1"))
  @Test def concat2 = assertEquals(BinOp(Concat(), Const("a"), Const("b")), parsing("=\"a\" & \"b\""))
  @Test def concat3 = assertEquals(BinOp(Concat(), BinOp(Concat(), Const("a"), Const("b")), Const("c")), parsing("=\"a\" & \"b\" & \"c\""))
  @Test def concat4 = assertEquals(BinOp(Concat(), BinOp(Concat(),BinOp(Concat(),Const("a"),Const("b")),Const("c")),Const("d")), parsing("=\"a\" & \"b\" & \"c\" & \"d\""))
  @Test def concat5 = assertEquals(BinOp(Concat(), Const("a"), BinOp(Concat(), Const("b"), BinOp(Concat(), Const("c"), Const("d")))), parsing("=\"a\" & (\"b\" & (\"c\" & \"d\"))"))
  @Test def concat6 = assertEquals(parsing("=\"a\" & \"b\" & \"c\" & \"d\""), parsing("=(((\"a\" & \"b\") & \"c\") & \"d\")"))

  @Test def add1 = assertEquals(BinOp(Plus(), Const(1), Const(1)), parsing("=1+1"))
  @Test def add2 = assertEquals(BinOp(Minus(), Const(1), Const(1)), parsing("=1-1"))
  @Test def add3 = assertEquals(BinOp(Plus(), BinOp(Minus(),Const(1), Const(2)), Const(3)), parsing("=1 - 2 + 3"))

  @Test def mul1 = assertEquals(BinOp(Mul(), Const(1), Const(1)), parsing("=1*1"))
  @Test def mul2 = assertEquals(BinOp(Div(), Const(1), Const(1)), parsing("=1/1"))
  @Test def mul3 = assertEquals(BinOp(Plus(), Const(1), BinOp(Mul(),Const(2), Const(3))), parsing("=1 + 2 * 3"))
  @Test def mul4 = assertEquals(BinOp(Plus(), Const(1), BinOp(Div(),Const(2), Const(3))), parsing("=1 + 2 / 3"))
  @Test def mul5 = assertEquals(BinOp(Div(), BinOp(Mul(),Const(1), Const(2)), Const(3)), parsing("=1 * 2 / 3"))

  // The following formula's are from http://homepages.mcs.vuw.ac.nz/~elvis/db/Excel.shtml
  def example01 {parsing("=1")}
  def example02 {parsing("=1+1")}
  def example03 {parsing("=A1")}
  def example04 {parsing("=$B$2")}
  def example05 {parsing("=SUM(B5:B15)")}
  def example06 {parsing("=SUM(B5:B15,D5:D15)")}
  def example07 {parsing("=SUM(B5:B15 A7:D7)")}
  def example08 {parsing("=SUM(sheet1!$A$1:$B$2)")}
  def example09 {parsing("=[data.xls]sheet1!$A$1")}
  def example10 {parsing("=SUM((A:A 1:1))")}
  def example11 {parsing("=SUM((A:A A1:B1))")}
  def example12 {parsing("=SUM((D9:D11,(E9:E11,F9:F11)))")}
  def example13 {parsing("=IF(P5=1.0,\"NA\",IF(P5=2.0,\"A\",IF(P5=3.0,\"B\",IF(P5=4.0,\"C\",IF(P5=5.0,\"D\",IF(P5=6.0,\"E\",IF(P5=7.0,\"F\",IF(P5=8.0,\"G\"))))))))")}
  // Enable if Array formula's are implemented
  //@Test def example14 {parsing("={SUM(B2:D2*B3:D3)}")}

  // The following formula's are from http://ewbi.blogs.com/develops/2004/12/excel_formula_p.html
  def complex1 {parsing("=IF(\"a\"={\"a\",\"b\";\"c\",#N/A;-1,TRUE}, \"yes\", \"no\") &   \"  more \"\"test\"\" text\"")}
  def complex2 {parsing("=IF(R13C3>DATE(2002,1,6),0,IF(ISERROR(R[41]C[2]),0,IF(R13C3>=R[41]C[2],0, IF(AND(R[23]C[11]>=55,R[24]C[11]>=20),R53C3,0))))")}
  def complex3 {parsing("=IF(R[39]C[11]>65,R[25]C[42],ROUND((R[11]C[11]*IF(OR(AND(R[39]C[11]>=55, R[40]C[11]>=20),AND(R[40]C[11]>=20,R11C3=\"YES\")),R[44]C[11],R[43]C[11]))+(R[14]C[11] *IF(OR(AND(R[39]C[11]>=55,R[40]C[11]>=20),AND(R[40]C[11]>=20,R11C3=\"YES\")), R[45]C[11],R[43]C[11])),0))")}
*/
}
