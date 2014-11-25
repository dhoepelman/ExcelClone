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

  def lstCtx(name: String, l: List[Tuple3[Any, String, Ctx]]) =
    l map (x => (name, tv(x._1), x._2, x._3))

  def lstErr(name: String, l: List[Tuple2[ErrType, String]]): List[TestTuple] =
    l map (x => (name, VErr(x._1), x._2, ectx))

  def newCtx(values: Map[String, Any]) = values map (x => {
    (p parsing("=" + x._1) match {
      case c: Cell => desugarCell(c)
      case _ => throw new IllegalArgumentException("oops")
    }, tv(x._2))
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
      )) ++ lst("binop <", List(
        (false,  "=2<1"),
        (false, "=1<1"),
        (true,  "=1<2"),
        (false, "=1<\"5\""),
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
      )) ++ lst("binop <=", List(
        (false, "=2<=1"),
        (true,  "=1<=1"),
        (true,  "=1<=2"),
        (false, "=1<=\"5\""),
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
      )) ++ lst("binop & concat", List(
        ("abc", "=\"ab\"& \"c\""),
        ("ab1", "=\"ab\"& 1"),
        ("1ab", "=1 & \"ab\""),
        ("TRUEab", "=TRUE & \"ab\""),
        ("12", "=1 & 2"),
        ("TRUEFALSE", "=TRUE & FALSE")
      )) ++ lst("binop + add", List(
        (5, "=2 + 3"),
        (2.4, "=1.4 + 1"),
        (-10, "=-20 + 10"),
        (-30, "=-20 + -10"),
        (5, "=4 + TRUE")
      )) ++ lstErr("binop + errors", List(
        (InvalidValue, "=2 + \"a\"")
      )) ++ lst("binop - minus", List(
        (1, "=3 - 2"),
        (0.5, "=1.5 - 1"),
        (-30, "=-10 - 20"),
        (-10, "=-20 - -10"),
        (1, "=2 - TRUE"),
        (-2, "=FALSE - 2")
      )) ++ lstErr("binop - errors", List(
        (InvalidValue, "=2 - \"a\"")
      )) ++ lst("binop * multiply", List(
        (6, "=2 * 3"),
        (5, "=2.5 * 2"),
        (4, "=4 * TRUE"),
        (0, "=FALSE * 3")
      )) ++ lstErr("binop * errors", List(
        (InvalidValue, "=2 * \"a\"")
      )) ++ lst("binop / divide", List(
        (2, "=6 / 3"),
        (2.5, "=10 / 4"),
        (0, "=0 / 1"),
        (2, "=2 / TRUE")
      )) ++ lstErr("binop / errors", List(
        (DivBy0, "=1 / 0"),
        (DivBy0, "=1 / FALSE"),
        (InvalidValue, "=2 / \"a\"")
      )) ++ lst("binop / divide", (
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
      )) ++ lstErr("unop", List(
        (InvalidValue, "=-\"A\""),
        (InvalidValue, "=\"A\"%")
      )) ++ lstErr("call unknown function", List(
        (InvalidName, "=FOOBAR11()")
      )) ++ lst("call SUM", List(
        (1, "=SUM(1)"),
        (4, "=SUM(1+1, 2)"),
        (10, "=SUM(1,2,3,4)")
      )) ++ lstErr("call SUM invalids", List(
        (InvalidValue, "=SUM(\"A\")")
      )) ++ lstCtx("cell reference", List(
        (4, "=A1", Map(ACell("A", 1) -> VDouble(4))),
        (8, "=A1*2", Map(ACell("A", 1) -> VDouble(4)))
      )) ++ lstCtx("a range of cells", List(
        (4, "=A1:A1", newCtx(Map("A1" -> 4))),
        (6, "=2+A1:A1", newCtx(Map("A1" -> 4))),
        (-4, "=-A1:A1", newCtx(Map("A1" -> 4))),
        (3, "=SUM(A1:A2)", newCtx(Map("A1" -> 1, "A2" -> 2))),
        (6, "=SUM(A1:A2,1+2)", newCtx(Map("A1" -> 1, "A2" -> 2))),
        (6, "=SUM(A1:C1)", newCtx(Map("A1" -> 1, "B1" -> 2, "C1" -> 3))),
        (10, "=SUM(A1:B2)", newCtx(Map("A1" -> 1, "B1" -> 2, "A2" -> 3, "B2" -> 4)))
      )) ++ lstErr("just a range", List(
        (InvalidValue, "=A1:A3"),
        (InvalidValue, "=2 + A1:A3"),
        (InvalidValue, "=A1:A3%")
      )) ++ lst("function ROWS", List(
        (4, "=ROWS(A2:A5)"),
        (1, "=ROWS(B31:B31)"),
        (10, "=ROWS(A14:A5)")
      )) ++ lst("function COLUMNS", List(
        (3, "=COLUMNS(A1:C1)"),
        (27, "=COLUMNS(A31:AA31)"),
        (4, "=COLUMNS(E14:B5)")
      )) ++ lst("function COUNT", List(
        (3, "=COUNT(TRUE, 1/0, 1, \"abc\", 10, 0)")
      )) ++ lstCtx("function AVG", List(
        (5, "=AVERAGE(A1:A2)", newCtx(Map("A1" -> 3, "A2" -> 7))),
        (2.5, "=AVERAGE(3, 1, 4, 2)", ectx),
        (5, "=AVERAGE(5, A1:A2)", newCtx(Map("A1" -> 3, "A2" -> 7)))
      )) ++ lst("function IF", List(
        (true, "=IF(TRUE)"),
        (false, "=IF(FALSE)"),
        (3, "=IF(TRUE, 3)"),
        (false, "=IF(FALSE, 3)"),
        (2, "=IF(TRUE,2,3)"),
        (3, "=IF(FALSE,2,3)"),
        (2, "=IF(1,2,3)"),
        (3, "=IF(0,2,3)")
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
      )) ++ lst("function NOT", List(
        (true,  "=NOT(FALSE)"),
        (false, "=NOT(TRUE)"),
        (true,  "=NOT(0)"),
        (false, "=NOT(1)")
      )) ++ lst("function POWER", List(
        (16, "=POWER(2,4)")
      )) ++ lst("function UPPER", List(
        ("ABC", "=UPPER(\"abc\")"),
        ("ABC", "=UPPER(\"aBc\")"),
        ("ABC", "=UPPER(\"ABC\")"),
        (1, "=UPPER(1)")
      )) ++ lst("function LOWER", List(
        ("abc", "=LOWER(\"abc\")"),
        ("abc", "=LOWER(\"aBc\")"),
        ("abc", "=LOWER(\"ABC\")"),
        (1, "=LOWER(1)")
      )) ++ lst("function LEN", List(
        (1, "=LEN(\"a\")"),
        (0, "=LEN(\"\")"),
        (7, "=LEN(\"abc def\")")
      )) ++ lst("function TRIM", List(
        ("", "=TRIM(\"\")"),
        ("abc", "=TRIM(\"abc\")"),
        ("abc", "=TRIM(\" abc \")"),
        ("abc", "=TRIM(\"   abc   \")")
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
