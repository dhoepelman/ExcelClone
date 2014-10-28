
package scalaExcel

import junit.framework.TestCase
import org.junit._
import org.junit.Assert._

class ParserTests {

  val p = new ExcelFormulaParser()

  def test(s1 : String, s2: String) = {
    val parsed1 = p parsing(s1)
    val parsed2 = p parsing(s2)
    if(parsed1 != parsed2) {
      throw new AssertionError(s"Expected <$s1> and <$s2> to have identical AST, was <$parsed1> and <$parsed2>")
    }
  }

  def test(e: Expr, s: String) = {
    val parsed = p parsing(s)
    if (parsed != e){
      throw new AssertionError(s"Expected <$s> to parse to <$e>, but was <$parsed>")
    }
  }

  private def assertFail(input: String) = {
    try {
      p parsing(input)
      fail(s"Parsed <$input> which should've failed")
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

  @Test def t1 = test(Const(100), "100")

  @Test def intLiteral = List(0, 1, 1025) foreach (i => {
    test(Const(i.toDouble), i.toDouble.toString)
    test(Const(i.toDouble), "=" + i.toString)
  })

  @Test def floatLiteral = List(500.0, 1.0, 0.0, 1.0, 1.0 / 3.0, Math.PI).foreach(i => {
    test(Const(i), i.toString) ;
    test(Const(i), "=" + i.toString)
  })

  @Test def unOpNegate = List(0, 1, 1024, 1.0 / 3.0) foreach (i => {
    test(Const(-i), "-" + i)
  })

  @Test def scientificNotationLiteral =
    List("1e3", "1.23E+10", "12e5", "1E+02", "8.7E-3")
      .foreach(i => {
        test(Const(i.toDouble), i)
        test(Const(i.toDouble), "=" + i)
        test(Const(-i.toDouble), "-" + i)
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
      test(Const(kv._2), kv._1)
      test(Const(kv._2), "=" + kv._1)
    })


  @Test def concat1 = test(BinOp(Concat(), Const(1), Const(1)), "=1 & 1")
  @Test def concat2 = test(BinOp(Concat(), Const("a"), Const("b")), "=\"a\" & \"b\"")
  @Test def concat3 = test(BinOp(Concat(), BinOp(Concat(), Const("a"), Const("b")), Const("c")), "=\"a\" & \"b\" & \"c\"")
  @Test def concat4 = test(BinOp(Concat(), BinOp(Concat(),BinOp(Concat(),Const("a"),Const("b")),Const("c")),Const("d")), "=\"a\" & \"b\" & \"c\" & \"d\"")
  @Test def concat5 = test(BinOp(Concat(), Const("a"), BinOp(Concat(), Const("b"), BinOp(Concat(), Const("c"), Const("d")))), "=\"a\" & (\"b\" & (\"c\" & \"d\"))")
  @Test def concat6 = test("=\"a\" & \"b\" & \"c\" & \"d\"", "=(((\"a\" & \"b\") & \"c\") & \"d\")")

  @Test def add1 = test(BinOp(Plus(), Const(1), Const(1)), "=1+1")
  @Test def add2 = test(BinOp(Minus(), Const(1), Const(1)), "=1-1")
  @Test def add3 = test(BinOp(Plus(), BinOp(Minus(),Const(1), Const(2)), Const(3)), "=1 - 2 + 3")

  @Test def mul1 = test(BinOp(Mul(), Const(1), Const(1)), "=1*1")
  @Test def mul2 = test(BinOp(Div(), Const(1), Const(1)), "=1/1")
  @Test def mul3 = test(BinOp(Plus(), Const(1), BinOp(Mul(),Const(2), Const(3))), "=1 + 2 * 3")
  @Test def mul4 = test(BinOp(Plus(), Const(1), BinOp(Div(),Const(2), Const(3))), "=1 + 2 / 3")
  @Test def mul5 = test(BinOp(Div(), BinOp(Mul(),Const(1), Const(2)), Const(3)), "=1 * 2 / 3")

  @Test def comp =
    Map (
      "=" -> Eq(),
      ">=" -> GTE(),
      "<=" -> LTE(),
      "<>" -> NEq(),
      ">" -> GT(),
      "<" -> LT()
    ) foreach (kv => {
      test(BinOp(kv._2, Const(1), Const(1)), "=1 " + kv._1 + " 1")
      test(BinOp(kv._2, Const(1), BinOp(Plus(), Const(1), Const(1))), "=1 " + kv._1 + " 1 + 1")
  })

  @Test def precedenceCompConcat = test("=1 = 1 & 1", "=1 = (1 & 1)")
  @Test def precedenceConcatAdd = test("=1 & 1 + 1", "=1 & (1+1)")
  @Test def precedenceAddMul = test("=1 + 5 * 3", "=1 + (5 * 3)")
  @Test def precedenceMulExp = test("=1 * 2^3", "=1 * (2^3)")
  @Test def precedenceExpPerc = test("=2%^3","=(2%)^3")
  @Test def precedencePercUMin = test("=-2%","=(-2)%")
  // Counterintuïtive, extra test
  @Test def precedenceExpUMin = test("=-2^2", "=(-2)^2")
  @Test def precedenceComplex1 = test("= \"a\" & \"b\" <= -20%^3", "= (\"a\" & \"b\") <= (((-20)%)^3)")

  private def cell(C : String, CA : Boolean, R : Int, RA : Boolean) = Cell(ColRef(C, CA), RowRef(R, RA))
  @Test def singleref =
    Map (
      "=B5" -> cell("B", false, 5, false),
      "=C$270" -> cell("C", false, 270, true),
      "=$FF100" -> cell("FF", true, 100, false),
      "=AZ$99" -> cell("AZ", false, 99, true)
    ) foreach (kv => {
      test(kv._2, kv._1)
      test(kv._2, kv._1.toLowerCase)
  })
  // -A1 is valid -(A1)
  // A-5 is valid in Excel, and is for us if we enable defined names. E.g. A-5 => (A) - 5
  @Test def invalidrefs = List("=$-A5", "=$+A5", "=$B+5", "=$B-5") foreach assertFail

  private def range(C1 : String, C1A : Boolean, R1 : Int, R1A : Boolean, C2 : String, C2A : Boolean, R2 : Int, R2A : Boolean) = Range(cell(C1, C1A, R1, R1A), cell(C2, C2A, R2, R2A))
  @Test def rangerefs =
    Map (
      "=A1:B2" -> range("A", false, 1, false, "B", false, 2, false),
      "=F$6:$C10" -> range("F", false, 6, false, "C", false, 10, false)
    ) foreach (kv => {
      test(kv._2, kv._1)
    })

  private def rowrange1 = test(RowRange(RowRef(1, false), RowRef(5, false)), "=5:5")
  private def rowrange2 = test(RowRange(RowRef(5000, true), RowRef(10000, false)), "=$5000:10000")
  private def colrange1 = test(ColRange(ColRef("A", true), ColRef("AZ", false)), "=A:AZ")
/*
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
