package scalaExcel.formula

import org.junit.Assert._
import org.junit._

import scalaExcel.formula.Values.{toVal => tv}

class ParserTests {

  def test(s1: String, s2: String) = {
    val parsed1 = removeParentheses(Parser parsing s1)
    val parsed2 = removeParentheses(Parser parsing s2)
    if(parsed1 != parsed2) {
      throw new AssertionError(s"Expected <$s1> and <$s2> to have identical AST, was <$parsed1> and <$parsed2>")
    }
  }

  def test(e: Expr, s: String) = {
    val parsed = removeParentheses(Parser parsing s)
    if (parsed != e){
      throw new AssertionError(s"Expected <$s> to parse to <$e>, but was <$parsed>")
    }
  }

  private def assertFail(input: String) = {
    try {
      Parser parsing input
      fail(s"Parsed <$input> which should've failed")
    } catch {
      case e: IllegalArgumentException =>
      case e: Exception => fail(e.getMessage)
    }
  }

  private def removeParentheses(e : Expr) : Expr = e match {
    case Group(e1) => removeParentheses(e1)
    case UnOp(op, e1) => UnOp(op, removeParentheses(e1))
    case BinOp(op, e1, e2) => BinOp(op, removeParentheses(e1), removeParentheses(e2))
    case Call(f, args) => Call(f, args.map(removeParentheses))
    case _ => e
  }

  @Test def empty() = test(Const(VEmpty), "")

  val consts = List[Any](100, 2.5, -10, "foo", true)

  @Test def doubleConstEquaility() = assertTrue(Const(tv(10.0)) equals Const(tv(10)))

  @Test def constEquality() = consts foreach
    (i => assertTrue(Const(tv(i)) equals Const(tv(i))))

  @Test def constAssertEquals() = consts foreach
    (i => assertEquals(Const(tv(i)), Const(tv(i))))

  @Test def t1() = test(Const(tv(100)), "100")

  @Test def intLiteral() = List(0, 1, 1025) foreach (i => {
    test(Const(tv(i.toDouble)), i.toDouble.toString)
    test(Const(tv(i.toDouble)), "=" + i.toString)
  })

  @Test def floatLiteral() = List(500.0, 1.0, 0.0, 1.0, 1.0 / 3.0, Math.PI).foreach(i => {
    test(Const(tv(i)), i.toString) ;
    test(Const(tv(i)), "=" + i.toString)
  })

  @Test def unOpNegate() = List(0, 1, 1024, 1.0 / 3.0) foreach (i => {
    test(Const(tv(-i)), "-" + i)
  })

  @Test def scientificNotationLiteral() =
    List("1e3", "1.23E+10", "12e5", "1E+02", "8.7E-3")
      .foreach(i => {
        test(Const(tv(i.toDouble)), i)
        test(Const(tv(i.toDouble)), "=" + i)
        test(Const(tv(-i.toDouble)), "-" + i)
      })

  @Test def invalidScientificNotationLiteral() =
    List("1.0e0.3", "1e1234", "1e-1234")
      .foreach (i => test(Const(VString(i)),i))

  @Test def booleanLiterals() =
    Map(
      "true" -> true,
      "TRUE" -> true,
      "truE" -> true,
      "false" -> false,
      "FALSE" -> false,
      "FaLSE" -> false
    ) foreach (kv => {
      test(Const(tv(kv._2)), kv._1)
      test(Const(tv(kv._2)), "=" + kv._1)
    })

  @Test def strings() = Map(
    "hello" -> "hello",
    "foo bar" -> "foo bar",
    "foo\"bar" -> "foo\"bar",
    "=\"hello\"" -> "hello",
    "=\"hello\"\"foo\"" -> "hello\"foo"
  ) foreach (kv => test(Const(tv(kv._2)), kv._1))

  @Test def stringStartingWithEquals() = assertFail("=hello")

  @Test def exprGroup() = test(Const(tv(1)), "=(1)")
  @Test def exprGroupPerc() = test(UnOp(Percent(), Const(tv(1))), "=(1%)")
  @Test def exprGroupPerc2() = test(UnOp(Percent(), Const(tv(1))), "=(1)%")

  @Test def concat1() = test(BinOp(Concat(), Const(tv(1)), Const(tv(1))), "=1 & 1")
  @Test def concat2() = test(BinOp(Concat(), Const(tv("a")), Const(tv("b"))), "=\"a\" & \"b\"")
  @Test def concat3() = test(BinOp(Concat(), BinOp(Concat(), Const(tv("a")), Const(tv("b"))), Const(tv("c"))), "=\"a\" & \"b\" & \"c\"")
  @Test def concat4() = test(BinOp(Concat(), BinOp(Concat(),BinOp(Concat(),Const(tv("a")),Const(tv("b"))),Const(tv("c"))),Const(tv("d"))), "=\"a\" & \"b\" & \"c\" & \"d\"")
  @Test def concat5() = test(BinOp(Concat(), Const(tv("a")), BinOp(Concat(), Const(tv("b")), BinOp(Concat(), Const(tv("c")), Const(tv("d"))))), "=\"a\" & (\"b\" & (\"c\" & \"d\"))")
  @Test def concat6() = test("=\"a\" & \"b\" & \"c\" & \"d\"", "=(((\"a\" & \"b\") & \"c\") & \"d\")")

  @Test def add1() = test(BinOp(Plus(), Const(tv(1)), Const(tv(1))), "=1+1")
  @Test def add2() = test(BinOp(Minus(), Const(tv(1)), Const(tv(1))), "=1-1")
  @Test def add3() = test(BinOp(Plus(), BinOp(Minus(),Const(tv(1)), Const(tv(2))), Const(tv(3))), "=1 - 2 + 3")

  @Test def mul1() = test(BinOp(Mul(), Const(tv(1)), Const(tv(1))), "=1*1")
  @Test def mul2() = test(BinOp(Div(), Const(tv(1)), Const(tv(1))), "=1/1")
  @Test def mul3() = test(BinOp(Plus(), Const(tv(1)), BinOp(Mul(),Const(tv(2)), Const(tv(3)))), "=1 + 2 * 3")
  @Test def mul4() = test(BinOp(Plus(), Const(tv(1)), BinOp(Div(),Const(tv(2)), Const(tv(3)))), "=1 + 2 / 3")
  @Test def mul5() = test(BinOp(Div(), BinOp(Mul(),Const(tv(1)), Const(tv(2))), Const(tv(3))), "=1 * 2 / 3")

  @Test def comp() =
    Map (
      "=" -> Eq(),
      ">=" -> GTE(),
      "<=" -> LTE(),
      "<>" -> NEq(),
      ">" -> GT(),
      "<" -> LT()
    ) foreach (kv => {
      test(BinOp(kv._2, Const(tv(1)), Const(tv(1))), "=1 " + kv._1 + " 1")
      test(BinOp(kv._2, Const(tv(1)), BinOp(Plus(), Const(tv(1)), Const(tv(1)))), "=1 " + kv._1 + " 1 + 1")
  })

  @Test def call0() = test(Call("SUM", List()), "=SUM()")
  @Test def call1() = test(Call("SUM", List(Const(tv(1)))), "=SUM(1)")
  @Test def call2() = test(Call("SUM", List(Const(tv(1)), Const(tv(2)))), "=SUM(1,2)")
  @Test def callMultiArg() = test(Call("SUM", List(Const(tv(1)), Const(tv(2)), Const(tv(3)))), "=SUM(1,2,3)")
  @Test def callExpArg() = test(Call("SUM", List(BinOp(Plus(), Const(tv(1)),Const(tv(1))), Const(tv(2)))), "=SUM(1 + 1,2)")
  @Test def callInCall() = test(Call("SUM", List(Call("SUM", List(Const(tv(1)))), Const(tv(2)))), "=SUM(SUM(1),2)")

  @Test def precedenceCall() = test("=SUM(1) + 3", "=(SUM(1)) + 3")

  @Test def precedenceCompConcat() = test("=1 = 1 & 1", "=1 = (1 & 1)")
  @Test def precedenceCompConcat2() = test("=1 & 1 = 1", "=(1 & 1) = 1")
  @Test def precedenceConcatAdd() = test("=1 & 1 + 1", "=1 & (1+1)")
  @Test def precedenceConcatAdd2() = test("=1 + 1 & 1", "=(1 + 1)&1")
  @Test def precedenceAddMul() = test("=1 + 5 * 3", "=1 + (5 * 3)")
  @Test def precedenceAddMul2() = test("=1 * 5 + 3", "=(1 * 5) + 3")
  @Test def precedenceMulExp() = test("=1 * 2^3", "=1 * (2^3)")
  @Test def precedenceMulExp2() = test("=1^2 * 3", "=(1^2)*3")
  @Test def precedenceExpPerc() = test("=2%^3","=(2%)^3")
  @Test def precedencePercUMin() = test("=-2%","=(-2)%")

  @Test def twoPercent() = test(UnOp(Percent(), UnOp(Percent(), Const(tv(1)))), "=1%%")
  @Test def multiPercent() = test("=((1%)%)%", "=1%%%")
  @Test def percentExpr() = test(UnOp(Percent(), UnOp(Plus(), Const(tv(1)))), "=+1%")
  @Test def unExpr() = test(UnOp(Plus(), Const(tv(1))), "=+1")

  // extra complex tests
  @Test def precedenceExpUMin() = test("=-2^2", "=(-2)^2")
  @Test def precedenceComplex1() = test("= \"a\" & \"b\" <= -20%^3", "= (\"a\" & \"b\") <= (((-20)%)^3)")


  private def cell(c: Int, ca: Boolean, r: Int, ra: Boolean) = Cell(ColRef(c, ca), RowRef(r, ra))
  @Test def singleref() =
    Map (
      "=B5" -> cell(1, false, 4, false),
      "=C$270" -> cell(2, false, 269, true),
      "=$FF100" -> cell(colToNum("FF"), true, 99, false),
      "=AZ$99" -> cell(51, false, 98, true),
      "=$ABCDE$12345" -> cell(colToNum("ABCDE"), true, 12344, true)
    ) foreach (kv => {
      test(kv._2, kv._1)
      test(kv._2, kv._1.toLowerCase)
  })
  // -A1 is valid -(A1)
  // A-5 is valid in Excel, and is for us if we enable defined names. E.g. A-5 => (A) - 5
  @Test def invalidrefs() = List("=$-A5", "=$+A5", "=$B+5", "=$B-5") foreach assertFail

  private def range(c1: Int, c1a: Boolean, r1: Int, r1a: Boolean, c2: Int, c2a: Boolean, r2: Int, r2a: Boolean)
    = Range(cell(c1, c1a, r1, r1a), cell(c2, c2a, r2, r2a))

  @Test def rangerefs() =
    Map (
      "=A1:B2" -> range(0, false, 0, false, 1, false, 1, false),
      "=F$6:$C10" -> range(5, false, 5, true, 2, true, 9, false)
    ) foreach (kv => {
      test(kv._2, kv._1)
    })

  @Test def rowrange1() = test(RowRange(RowRef(0, false), RowRef(4, false)), "=1:5")
  @Test def rowrange2() = test(RowRange(RowRef(4999, true), RowRef(9999, false)), "=$5000:10000")
  @Test def colrange1() = test(ColRange(ColRef(0, false), ColRef(colToNum("AZ"), false)), "=A:AZ")

  @Test def callref1() = test(Call("SUM", List(cell(0, false, 0, false))), "=SUM(A1)")

  // The following formula's are from http://homepages.mcs.vuw.ac.nz/~elvis/db/Excel.shtml
  @Test def example01() {Parser parsing("=1")}
  @Test def example02() {Parser parsing("=1+1")}
  @Test def example03() {Parser parsing("=A1")}
  @Test def example04() {Parser parsing("=$B$2")}
  @Test def example05() {Parser parsing("=SUM(B5:B15)")}
  @Test def example06() {Parser parsing("=SUM(B5:B15,D5:D15)")}
  // Enable if we allow complex ranges
  //@Test def example07() {Parser parsing("=SUM(B5:B15 A7:D7)")}
  @Test def example08() {Parser parsing("=SUM(sheet1!$A$1:$B$2)")}
  // Enable if we allow cross-file references
  //@Test def example09() {Parser parsing("=[data.xls]sheet1!$A$1")}
  // Enable if we allow complex ranges
  //@Test def example10() {Parser parsing("=SUM((A:A 1:1))")}
  //@Test def example11() {Parser parsing("=SUM((A:A A1:B1))")}
  //@Test def example12() {Parser parsing("=SUM((D9:D11,(E9:E11,F9:F11)))")}
  @Test def example13() {Parser parsing("=IF(P5=1.0,\"NA\",IF(P5=2.0,\"A\",IF(P5=3.0,\"B\",IF(P5=4.0,\"C\",IF(P5=5.0,\"D\",IF(P5=6.0,\"E\",IF(P5=7.0,\"F\",IF(P5=8.0,\"G\"))))))))")}
  // Enable if Array formula's are implemented
  //@Test def example14() {parsing("={SUM(B2:D2*B3:D3)}")}

  // The following formula's are from http://ewbi.blogs.com/develops/2004/12/excel_formula_p.html
  // Enable if Array formula's are implemented
  //@Test def complex1() {Parser parsing("=IF(\"a\"={\"a\",\"b\";\"c\",#N/A;-1,TRUE}, \"yes\", \"no\") &   \"  more \"\"test\"\" text\"")}
  // Enable if structured references are implemented
  //@Test def complex2() {Parser parsing("=IF(R13C3>DATE(2002,1,6),0,IF(ISERROR(R[41]C[2]),0,IF(R13C3>=R[41]C[2],0, IF(AND(R[23]C[11]>=55,R[24]C[11]>=20),R53C3,0))))")}
  //@Test def complex3() {Parser parsing("=IF(R[39]C[11]>65,R[25]C[42],ROUND((R[11]C[11]*IF(OR(AND(R[39]C[11]>=55, R[40]C[11]>=20),AND(R[40]C[11]>=20,R11C3=\"YES\")),R[44]C[11],R[43]C[11]))+(R[14]C[11] *IF(OR(AND(R[39]C[11]>=55,R[40]C[11]>=20),AND(R[40]C[11]>=20,R11C3=\"YES\")), R[45]C[11],R[43]C[11])),0))")}

  @Test def whitespaces() {Parser parsing("=\n 1 \t + \t\t\n 4")}

  @Test def string1() = test(Const(VString("abc")), "abc")
  @Test def string2() = test(Const(VString("1 + 1")), "1 + 1")
  @Test def string3() = test(Const(VString("12A")), "12A")
  @Test def string4() = test(Const(VString("A%")), "A%")
  @Test def string5() = test(Const(VString("+A")), "+A")

}
