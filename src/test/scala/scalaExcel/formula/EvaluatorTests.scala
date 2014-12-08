package scalaExcel.formula

import java.{util => ju, lang => jl}
import org.junit.Assert._
import org.junit.Test
import org.junit.runner.RunWith
import org.junit.runners.Parameterized
import org.junit.runners.Parameterized.Parameters

import math.pow
import scalaExcel.formula.Evaluator.{eval, Ctx, desugarCell}
import scalaExcel.formula.Values.{toVal => tv}

@RunWith(value = classOf[Parameterized])
class EvaluatorTests(name: String, e: Value, v: Any, ctx: Ctx) {

  val p = new Parser()

  @Test def x() = v match {
    case v: Expr   => assertEquals(e, eval(ctx, v))
    case v: String => assertEquals(e, eval(ctx, p parsing v))
    case _ => throw new IllegalArgumentException("Can't test something else")
  }

}

object EvaluatorTests {

  type TestTuple = (String, Value, AnyRef, Ctx)

  val p = new Parser

  val ectx = Map[ACell, Value]();

  def lst(name: String, l: List[Tuple2[Any, String]]) =
    lstCtx(name, l map (x => (x._1, x._2, ectx)))

  def lstErr(name: String, l: List[Tuple2[ErrType, String]]): List[TestTuple] =
    lstErrCtx(name, l map (x => (x._1, x._2, ectx)))

  def lstCtx(name: String, l: List[Tuple3[Any, String, Ctx]]) =
    l map (x => (name, tv(x._1), x._2, x._3))

  def lstErrCtx(name: String, l: List[Tuple3[ErrType, String, Ctx]]) =
    l map (x => (name, VErr(x._1), x._2, x._3))

  def newCtx(values: Map[String, Any]) = values map (x => {
    (p parsing("=" + x._1) match {
      case c: Cell => desugarCell(c)
      case _ => throw new IllegalArgumentException("oops")
    }, tv(x._2))
  })

  val sparseCtx: Ctx = (c => {
    val (ACell((x, y))) = c
    if (y == 0 || y == 3 || y == 5) VDouble(3)
    else VEmpty
  })

  @Parameters(name= "{0}: <{1}> : <{2}>")
  def data: ju.Collection[Array[jl.Object]] = {
    val list = new ju.ArrayList[Array[jl.Object]]()
    (
      (List[TestTuple](
        ("evalConst", VDouble(10), Const(tv(10)), ectx),
        ("evalConst", VBool(true), Const(tv(true)), ectx),
        ("evalConst", VBool(false), Const(tv(false)), ectx),
        ("evalConst", VString("hi"), Const(tv("hi")), ectx),
        ("evalConst", VString("hi"), "=\"hi\"", ectx),
        ("evalConst Err", VErr(NA), Const(VErr(NA)), ectx)
      )) ++ lst("binop =", List(
        (true, "=TRUE = TRUE"),
        (false, "=FALSE = TRUE"),
        (false, "=TRUE = FALSE"),
        (true, "=FALSE = FALSE"),
        (true, "=1 = 1"),
        (false, "=1 = 2"),
        (true, "=\"a\" = \"a\""),
        (false, "=\"a\" = \"b\""),
        (false, "=\"a\" = 1"),
        (false, "=\"a\" = TRUE"),
        (false, "=1 = TRUE"),
        (false, "=TRUE = 1")
      )) ++ lstCtx("binop =", List(
        (false, "=1 = A20", (_ => VEmpty)),
        (true,  "=A20 = 0", (_ => VEmpty))
      )) ++ lst("binop <>", List(
        (!true, "=TRUE <> TRUE"),
        (!false, "=FALSE <> TRUE"),
        (!false, "=TRUE <> FALSE"),
        (!true, "=FALSE <> FALSE"),
        (!true, "=1 <> 1"),
        (!false, "=1 <> 2"),
        (!true, "=\"a\" <> \"a\""),
        (!false, "=\"a\" <> \"b\""),
        (!false, "=\"a\" <> 1"),
        (!false, "=\"a\" <> TRUE"),
        (!false, "=1 <> TRUE"),
        (true, "= TRUE <> 1")
      )) ++ lstCtx("binop <>", List(
        (true, "=1 <> A20", (_ => VEmpty)),
        (false,  "=A20 <> 0", (_ => VEmpty))
      )) ++ lst("binop >", List(
        (true,  "=2>1"),
        (false, "=1>1"),
        (false, "=1>2"),
        (false, "=1>\"5\""),
        (false, "=1>TRUE"),
        (false, "=1>FALSE"),
        (true,  "=TRUE>1"),
        (true,  "=FALSE>1"),
        (false, "=FALSE>FALSE"),
        (false, "=TRUE>TRUE"),
        (true,  "=TRUE>FALSE"),
        (false, "=FALSE>TRUE"),
        (true,  "=\"b\">\"a\""),
        (false, "=\"a\">\"b\""),
        (false, "=\"b\">\"b\"")
      )) ++ lstCtx("binop >", List(
        (true, "=1 > A20", (_ => VEmpty)),
        (false,  "=A20 > 1", (_ => VEmpty))
      )) ++ lst("binop <", List(
        (false,  "=2<1"),
        (false, "=1<1"),
        (true,  "=1<2"),
        (true,  "=1<\"5\""),
        (true,  "=1<TRUE"),
        (true,  "=1<FALSE"),
        (false, "=TRUE<1"),
        (false, "=FALSE<1"),
        (false, "=TRUE<1"),
        (false, "=FALSE<1"),
        (false, "=FALSE<FALSE"),
        (false, "=TRUE<TRUE"),
        (false, "=TRUE<FALSE"),
        (true,  "=FALSE<TRUE"),
        (false, "=\"b\"<\"a\""),
        (true,  "=\"a\"<\"b\""),
        (false, "=\"b\"<\"b\"")
      )) ++ lstCtx("binop <", List(
        (false, "=1 < A20", (_ => VEmpty)),
        (true,  "=A20 < 1", (_ => VEmpty))
      )) ++ lst("binop >=", List(
        (true,  "=2>=1"),
        (true,  "=1>=1"),
        (false, "=1>=2"),
        (false, "=1>=\"5\""),
        (false, "=1>=TRUE"),
        (false, "=1>=FALSE"),
        (true,  "=TRUE>=1"),
        (true,  "=FALSE>=1"),
        (true,  "=FALSE>=FALSE"),
        (true,  "=TRUE>=TRUE"),
        (true,  "=TRUE>=FALSE"),
        (false, "=FALSE>=TRUE"),
        (true,  "=\"b\">=\"a\""),
        (false, "=\"a\">=\"b\""),
        (true,  "=\"b\">=\"b\"")
      )) ++ lstCtx("binop >=", List(
        (true, "=1 >= A20", (_ => VEmpty)),
        (false,  "=A20 >= 1", (_ => VEmpty))
      )) ++ lst("binop <=", List(
        (false, "=2<=1"),
        (true,  "=1<=1"),
        (true,  "=1<=2"),
        (true,  "=1<=\"5\""),
        (true,  "=1<=TRUE"),
        (true,  "=1<=FALSE"),
        (false, "=TRUE<=1"),
        (false, "=FALSE<=1"),
        (false, "=TRUE<=1"),
        (false, "=FALSE<=1"),
        (true,  "=FALSE<=FALSE"),
        (true,  "=TRUE<=TRUE"),
        (false, "=TRUE<=FALSE"),
        (true,  "=FALSE<=TRUE"),
        (false, "=\"b\"<=\"a\""),
        (true,  "=\"a\"<=\"b\""),
        (true,  "=\"b\"<=\"b\"")
      )) ++ lstCtx("binop <=", List(
        (false, "=1 <= A20", (_ => VEmpty)),
        (true,  "=A20 <= 1", (_ => VEmpty))
      )) ++ lst("binop & concat", List(
        ("abc", "=\"ab\"& \"c\""),
        ("ab1", "=\"ab\"& 1"),
        ("1ab", "=1 & \"ab\""),
        ("TRUEab", "=TRUE & \"ab\""),
        ("12", "=1 & 2"),
        ("TRUEFALSE", "=TRUE & FALSE")
      )) ++ lstCtx("binop & concat", List(
        ("ab", "=\"ab\" & A20", (_ => VEmpty)),
        ("ab", "=A20 & \"ab\"", (_ => VEmpty))
      )) ++ lst("binop + add", List(
        (5, "=2 + 3"),
        (2.4, "=1.4 + 1"),
        (-10, "=-20 + 10"),
        (-30, "=-20 + -10"),
        (5, "=4 + TRUE")
      )) ++ lstCtx("binop + add", List(
        (1, "=1 + A20", (_ => VEmpty)),
        (2,  "=A20 + 2", (_ => VEmpty))
      )) ++ lstErr("binop + errors", List(
        (InvalidValue, "=2 + \"a\"")
      )) ++ lst("binop - minus", List(
        (1, "=3 - 2"),
        (0.5, "=1.5 - 1"),
        (-30, "=-10 - 20"),
        (-10, "=-20 - -10"),
        (1, "=2 - TRUE"),
        (-2, "=FALSE - 2")
      )) ++ lstCtx("binop - minus", List(
        (1, "=1 - A20", (_ => VEmpty)),
        (-2,  "=A20 - 2", (_ => VEmpty))
      )) ++ lstErr("binop - errors", List(
        (InvalidValue, "=2 - \"a\"")
      )) ++ lst("binop * multiply", List(
        (6, "=2 * 3"),
        (5, "=2.5 * 2"),
        (4, "=4 * TRUE"),
        (0, "=FALSE * 3")
      )) ++ lstCtx("binop * multiply", List(
        (0, "=1 * A20", (_ => VEmpty)),
        (0,  "=A20 * 2", (_ => VEmpty))
      )) ++ lstErr("binop * errors", List(
        (InvalidValue, "=2 * \"a\"")
      )) ++ lst("binop / divide", List(
        (2, "=6 / 3"),
        (2.5, "=10 / 4"),
        (0, "=0 / 1"),
        (2, "=2 / TRUE")
      )) ++ lstCtx("binop * multiply", List(
        (0,  "=A20 / 2", (_ => VEmpty))
      )) ++ lstErr("binop / errors", List(
        (DivBy0, "=1 / 0"),
        (DivBy0, "=1 / FALSE"),
        (InvalidValue, "=2 / \"a\"")
      )) ++ lstErrCtx("binop / divide", List(
        (DivBy0,  "=2 / A20", (_ => VEmpty))
      )) ++ lst("binop ^ power", (
        (Map[Double,List[Double]](
          -2.0 -> List(-2, -1, 0, 1, 2),
          -0.5 -> List(-2, -1, 0, 1, 2),
          0.0  -> List(0.5, 1, 2),
          0.5  -> List(-2, -1, -0.5, 0, 1, 0.5, 2),
          1.0  -> List(-2, -1, -0.5, 0, 1, 0.5, 2),
          2.0  -> List(-2, -1, -0.5, 0, 1, 0.5, 2)
        ) map (x => {
            (x._1, x._2 map (y => (pow(x._1, y), "=" + x._1 + s"^$y")))
          })
        ).values.toList.flatten ++ List(
          (1, "=TRUE^1"),
          (0, "=FALSE^1")
        )
      )) ++ lstErr("binop ^ errors", List(
        (DivBy0, "=0^-2"),
        (DivBy0, "=0^-1"),
        (DivBy0, "=0^-0.5"),
        (NotNumeric, "=0^0"),
        (NotNumeric, "=-2^-0.5"),
        (NotNumeric, "=-0.5^-0.5"),
        (NotNumeric, "=-2^0.5"),
        (NotNumeric, "=-0.5^0.5"),
        (InvalidValue, "=\"a\"^2"),
        (InvalidValue, "=2^\"a\"")
      )) ++ lst("unop", List(
        (5, "=+5"),
        ("A", "=+\"A\""),
        (-5, "=-5"),
        (true, "=+TRUE"),
        (false, "=+FALSE"),
        (-1, "=-TRUE"),
        (0, "=-FALSE"),
        (0.25, "=25%"),
        (0.0025, "=25%%"),
        (0.01, "=TRUE%"),
        (0, "=FALSE%")
      )) ++ lstCtx("binop * multiply", List(
        (0, "=A20%", (_ => VEmpty)),
        (0, "=-A20", (_ => VEmpty)),
        (0,  "=+A20", (_ => VEmpty))
      )) ++ lstErr("unop", List(
        (InvalidValue, "=-\"A\""),
        (InvalidValue, "=\"A\"%")
      )) ++ lstCtx("cell reference", List(
        (4, "=A1", Map(ACell((0, 0)) -> VDouble(4))),
        (8, "=A1*2", Map(ACell((0, 0)) -> VDouble(4)))
      )) ++ lstErr("just a range", List(
        (InvalidValue, "=A1:A3"),
        (InvalidValue, "=2 + A1:A3"),
        (InvalidValue, "=A1:A3%")
      )) ++ lstErr("call unknown function", List(
        (InvalidName, "=FOOBAR11()")
      )) ++ lst("call SUM", List(
        (1, "=SUM(1)"),
        (4, "=SUM(1+1, 2)"),
        (10, "=SUM(1,2,3,4)")
      )) ++ lstErr("call SUM invalids", List(
        (InvalidValue, "=SUM(\"A\")")
      )) ++ lstCtx("a range of cells", List(
        (4, "=A1:A1", newCtx(Map("A1" -> 4))),
        (6, "=2+A1:A1", newCtx(Map("A1" -> 4))),
        (-4, "=-A1:A1", newCtx(Map("A1" -> 4))),
        (3, "=SUM(A1:A2)", newCtx(Map("A1" -> 1, "A2" -> 2))),
        (6, "=SUM(A1:A2,1+2)", newCtx(Map("A1" -> 1, "A2" -> 2))),
        (6, "=SUM(A1:C1)", newCtx(Map("A1" -> 1, "B1" -> 2, "C1" -> 3))),
        (10, "=SUM(A1:B2)", newCtx(Map("A1" -> 1, "B1" -> 2, "A2" -> 3, "B2" -> 4)))
      )) ++ lstCtx("function AVERAGE", List(
        (5, "=AVERAGE(A1:A2)", newCtx(Map("A1" -> 3, "A2" -> 7))),
        (2.5, "=AVERAGE(3, 1, 4, 2)", ectx),
        (5, "=AVERAGE(5, A1:A2)", newCtx(Map("A1" -> 3, "A2" -> 7))),
        (3, "=AVERAGE(A1:A5)", sparseCtx)
      )) ++ lst("function POWER", List(
        (16, "=POWER(2,4)")
      )) ++ lst("function ROUND", List(
        (123,     "=ROUND(123.456)"),
        (123.5,   "=ROUND(123.456, 1)"),
        (123.46,  "=ROUND(123.456, 2)"),
        (123.456, "=ROUND(123.456, 4)"),
        (120,     "=ROUND(123.456, -1)"),
        (100,     "=ROUND(123.456, -2)")
      )) ++ lstErr("function ROUND", List(
        (InvalidValue, """=ROUND("A")"""),
        (InvalidValue, """=ROUND(1, "A")""")
      )) ++ lst("function ROW", List(
        (2, "=ROW(A2)"),
        (31, "=ROW(B31:B66)"),
        (5, "=ROW(A14:A5)")
      )) ++ lst("function ROWS", List(
        (1, "=ROWS(A2)"),
        (4, "=ROWS(A2:A5)"),
        (1, "=ROWS(B31:B31)"),
        (10, "=ROWS(A14:A5)")
      )) ++ lst("function COLUMN", List(
        (1, "=COLUMN(A2)"),
        (3, "=COLUMN(C1:Z1)"),
        (27, "=COLUMN(ZZ31:AA31)"),
        (2, "=COLUMN(E14:B5)")
      )) ++ lst("function COLUMNS", List(
        (1, "=COLUMNS(A2)"),
        (3, "=COLUMNS(A1:C1)"),
        (27, "=COLUMNS(A31:AA31)"),
        (4, "=COLUMNS(E14:B5)")
      )) ++ lst("function COUNT", List(
        (3, "=COUNT(TRUE, 1/0, 1, \"abc\", 10, 0)")
      )) ++ lstCtx("function COUNT", List(
        (2, "=COUNT(A1:A5)", sparseCtx)
      )) ++ lstCtx("function MATCH", List(
        (1, "=MATCH(4, B6)", newCtx(Map("B6" -> 4))),
        (1, "=MATCH(4, A1:A3)", newCtx(Map("A1" -> 4, "A2" -> 5, "A3" -> 6))),
        (2, "=MATCH(5, A1:A3)", newCtx(Map("A1" -> 4, "A2" -> 5, "A3" -> 6))),
        (3, "=MATCH(6, A1:A3)", newCtx(Map("A1" -> 4, "A2" -> 5, "A3" -> 6))),
        (3, "=MATCH(6, C1:E1)", newCtx(Map("C1" -> 4, "D1" -> 5, "E1" -> 6))),
        (3, "=MATCH(6, A3:A1)", newCtx(Map("A1" -> 4, "A2" -> 5, "A3" -> 6))),
        (3, "=MATCH(6, E1:C1)", newCtx(Map("C1" -> 4, "D1" -> 5, "E1" -> 6)))
      )) ++ lstErrCtx("function MATCH invalids", List(
        (NA,           """=MATCH(4, A1:A3, 0)""", newCtx(Map("A1" -> 1, "A2" -> 2, "A3" -> 3))),
        (NA,           """=MATCH(1, A1:B3, 0)""", newCtx(Map("A1" -> 1, "A2" -> 2, "A3" -> 3))),
        (InvalidValue, """=MATCH(1, "A", 0)""",   newCtx(Map("A1" -> 1, "A2" -> 2, "A3" -> 3)))
      )) ++ lstCtx("function VLOOKUP", List(
        (1, "=VLOOKUP(1, A1:A3, 1)", newCtx(Map("A1" -> 1, "A2" -> 2, "A3" -> 3))),
        (2, "=VLOOKUP(2, A1:A3, 1)", newCtx(Map("A1" -> 1, "A2" -> 2, "A3" -> 3))),
        (3, "=VLOOKUP(3, A1:A3, 1)", newCtx(Map("A1" -> 1, "A2" -> 2, "A3" -> 3))),
        (4, "=VLOOKUP(1, A1:B3, 2)", newCtx(Map("A1" -> 1, "A2" -> 2, "A3" -> 3, "B1" -> 4))),
        (4, "=VLOOKUP(1, B3:A1, 2)", newCtx(Map("A1" -> 1, "A2" -> 2, "A3" -> 3, "B1" -> 4))),
        (3, "=VLOOKUP(3, B5, 1)",    newCtx(Map("B5" -> 3))),
        (3, "=VLOOKUP(3, B5, 1.6)",  newCtx(Map("B5" -> 3))),
        (3, "=VLOOKUP(A1, B5, 1)",   newCtx(Map("A1" -> 3, "B5" -> 3)))
      )) ++ lstErrCtx("function VLOOKUP invalids", List(
        (NA,           """=VLOOKUP(4, A1:A3, 1)""",   newCtx(Map("A1" -> 1, "A2" -> 2, "A3" -> 3))),
        (NA,           """=VLOOKUP(4, A1:A3, 2)""",   newCtx(Map("A1" -> 1, "A2" -> 2, "A3" -> 3))),
        (InvalidRef,   """=VLOOKUP(2, A1:A3, 2)""",   newCtx(Map("A1" -> 1, "A2" -> 2, "A3" -> 3))),
        (InvalidValue, """=VLOOKUP(2, A1:A3, "A")""", newCtx(Map("A1" -> 1))),
        (InvalidValue, """=VLOOKUP(2, 1, 1)""",       newCtx(Map("A1" -> 1)))
      )) ++ lst("function ADDRESS", List(
        ("$A$1", "=ADDRESS(1,1)"),
        ("$A$1", "=ADDRESS(1,1)"),
        ("$E$3", "=ADDRESS(3,5)"),
        ("$E$3", "=ADDRESS(3,5,1)"),
        ("E$3",  "=ADDRESS(3,5,2)"),
        ("$E3",  "=ADDRESS(3,5,3)"),
        ("E3",   "=ADDRESS(3,5,4)")
      )) ++ lstErr("function VLOOKUP invalids", List(
        (InvalidValue, "=ADDRESS(0,1)"),
        (InvalidValue, "=ADDRESS(1,0)"),
        (InvalidValue, "=ADDRESS(1,1,0)"),
        (InvalidValue, "=ADDRESS(1,1,5)")
      )) ++ lst("function IF", List(
        (true, "=IF(TRUE)"),
        (false, "=IF(FALSE)"),
        (3, "=IF(TRUE, 3)"),
        (false, "=IF(FALSE, 3)"),
        (2, "=IF(TRUE,2,3)"),
        (3, "=IF(FALSE,2,3)"),
        (2, "=IF(1,2,3)"),
        (3, "=IF(0,2,3)")
      )) ++ lstCtx("IF with empty", List(
        (2, "=IF(B2,1,2)", (_ => VEmpty))
      )) ++ lstErr("function IF", List(
        (InvalidValue, "=IF(\"A\")")
      )) ++ lst("function OR", List(
        (true,  "=OR(TRUE, TRUE)"),
        (true,  "=OR(FALSE, TRUE)"),
        (true,  "=OR(TRUE, FALSE)"),
        (false, "=OR(FALSE, FALSE)"),
        (true,  "=OR(1, 1)"),
        (true,  "=OR(0, 1)"),
        (true,  "=OR(1, 0)"),
        (false, "=OR(0, 0)"),
        (true,  "=OR(0,0,0,1,0)"),
        (false, "=OR(0,0,0,0,0)"),
        (true,  "=OR(1,1,1,1,1)")
      )) ++ lstCtx("OR with empty", List(
        (false, "=OR(B1, 0)", (_ => VEmpty)),
        (true, "=OR(B1, 1)", (_ => VEmpty))
      )) ++ lst("function AND", List(
        (true,  "=AND(TRUE, TRUE)"),
        (false, "=AND(FALSE, TRUE)"),
        (false, "=AND(TRUE, FALSE)"),
        (false, "=AND(FALSE, FALSE)"),
        (true,  "=AND(1, 1)"),
        (false, "=AND(0, 1)"),
        (false, "=AND(1, 0)"),
        (false, "=AND(0, 0)"),
        (false, "=AND(0,0,0,1,0)"),
        (false, "=AND(0,0,0,0,0)"),
        (true,  "=AND(1,1,1,1,1)")
      )) ++ lstCtx("AND with empty", List(
        (false, "=AND(B1, 1)", (_ => VEmpty))
      )) ++ lst("function NOT", List(
        (true,  "=NOT(FALSE)"),
        (false, "=NOT(TRUE)"),
        (true,  "=NOT(0)"),
        (false, "=NOT(1)")
      )) ++ lstCtx("NOT with empty", List(
        (true, "=NOT(B1)", (_ => VEmpty))
      )) ++ lst("function UPPER", List(
        ("ABC", "=UPPER(\"abc\")"),
        ("ABC", "=UPPER(\"aBc\")"),
        ("ABC", "=UPPER(\"ABC\")"),
        ("TRUE", "=UPPER(TRUE)"),
        ("1", "=UPPER(1)")
      )) ++ lstCtx("UPPER empty", List(
        ("", "=UPPER(A1)", (_ => VEmpty))
      )) ++ lst("function LOWER", List(
        ("abc", "=LOWER(\"abc\")"),
        ("abc", "=LOWER(\"aBc\")"),
        ("abc", "=LOWER(\"ABC\")"),
        ("true", "=LOWER(TRUE)"),
        ("1", "=LOWER(1)")
      )) ++ lstCtx("LOWER empty", List(
        ("", "=LOWER(A1)", (_ => VEmpty))
      )) ++ lst("function LEN", List(
        (1, "=LEN(\"a\")"),
        (0, "=LEN(\"\")"),
        (7, "=LEN(\"abc def\")"),
        (4, "=LEN(TRUE)"),
        (3, "=LEN(123)")
      )) ++ lstCtx("LEN empty", List(
        (0, "=LEN(A1)", (_ => VEmpty))
      )) ++ lst("function TRIM", List(
        ("", "=TRIM(\"\")"),
        ("abc", "=TRIM(\"abc\")"),
        ("abc", "=TRIM(\" abc \")"),
        ("abc", "=TRIM(\"   abc   \")"),
        ("TRUE", "=TRIM(TRUE)"),
        ("123", "=TRIM(123)")
      )) ++ lstCtx("TRIM empty", List(
        ("", "=TRIM(A1)", (_ => VEmpty))
      )) ++ lst("grouping", List(
        (10, "=(1 + 4) * 2"),
        (5, "=(2+3)")
      )) ++ lstCtx("grouping", List(
        (6, "=SUM((A1:A2),1+2)", newCtx(Map("A1" -> 1, "A2" -> 2)))
      ))) foreach ({
      case (a, b, c, ctx) => list.add(Array(a, b, c, ctx))
    })

    return list
  }

}
