
package scalaExcel.formula

import java.{util => ju, lang => jl}
import org.junit.Assert._
import org.junit.Test
import org.junit.runner.RunWith
import org.junit.runners.Parameterized
import org.junit.runners.Parameterized.Parameters

import scalaExcel.formula.Evaluator.eval
import scalaExcel.formula.Values.{toVal => tv}

@RunWith(value = classOf[Parameterized])
class EvaluatorTests(name: String, e: Value, v: Any) {

  val p = new Parser()

  @Test def x = v match {
    case v: Expr   => assertEquals(e, eval(v))
    case v: String => assertEquals(e, eval(p parsing v))
    case _ => throw new IllegalArgumentException("Can't test something else")
  }

}

object EvaluatorTests {

  def lst(name: String, l: List[Tuple2[Any, String]]) =
    l map (x => (name, tv(x._1), x._2))

  def lstErr(name: String, l: List[Tuple2[ErrType, String]]) =
    l map (x => (name, VErr(x._1), x._2))

  @Parameters(name= "{0}: <{1}> : <{2}>")
  def data: ju.Collection[Array[jl.Object]] = {
    val list = new ju.ArrayList[Array[jl.Object]]()
    (
      (List[Tuple3[String, Value, AnyRef]](
        ("evalConst", VDouble(10), Const(tv(10))),
        ("evalConst", VBool(true), Const(tv(true))),
        ("evalConst", VBool(false), Const(tv(false))),
        ("evalConst", VString("hi"), Const(tv("hi"))),
        ("evalConst", VString("hi"), "=\"hi\"")
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
        (-30, "=-20 + -10")
      )) ++ lstErr("binop + errors", List(
        (NotNumeric(), "=2 + \"a\""),
        (NotNumeric(), "=2 + TRUE")
      )) ++ lst("binop - minus", List(
        (1, "=3 - 2"),
        (0.5, "=1.5 - 1"),
        (-30, "=-10 - 20"),
        (-10, "=-20 - -10")
      )) ++ lstErr("binop - errors", List(
        (NotNumeric(), "=2 - \"a\""),
        (NotNumeric(), "=2 - TRUE")
      )) ++ lst("binop * multiply", List(
        (6, "=2 * 3"),
        (5, "=2.5 * 2")
      )) ++ lstErr("binop * errors", List(
        (NotNumeric(), "=2 * \"a\""),
        (NotNumeric(), "=2 * TRUE")
      )) ++ lst("binop / divide", List(
        (2, "=6 / 3"),
        (2.5, "=10 / 4"),
        (0, "=0 / 1")
      )) ++ lstErr("binop / errors", List(
        (DivBy0(), "=1 / 0"),
        (NotNumeric(), "=2 / \"a\""),
        (NotNumeric(), "=2 / TRUE")
      )) ++ lst("unop", List(
        (5, "=+5"),
        (-5, "=-5"),
        (0.25, "=25%"),
        (0.0025, "=25%%")
      )) ++ lstErr("unop", List(
        (NotNumeric(), "=+\"A\""),
        (NotNumeric(), "=-\"A\""),
        (NotNumeric(), "=\"A\"%")
      ))
    ) foreach (x => list.add(Array(x._1, x._2, x._3)))
    return list
  }

}
