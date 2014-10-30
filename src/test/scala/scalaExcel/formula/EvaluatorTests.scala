
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

  @Parameters(name= "{0}: <{1}>=<{2}>")
  def data: ju.Collection[Array[jl.Object]] = {
    val list = new ju.ArrayList[Array[jl.Object]]()
    (List[Tuple3[String, Value, AnyRef]](
      ("evalConst", VDouble(10), Const(tv(10))),
      ("evalConst", VBool(true), Const(tv(true))),
      ("evalConst", VBool(false), Const(tv(false))),
      ("evalConst", VString("hi"), Const(tv("hi"))),
      ("evalConst", VString("hi"), "=\"hi\""),

      ("binOpSum",       VDouble(5), "=2 + 3"),
      ("binOpSumString", VErr(NotNumeric()), "=2 + \"a\""),
      ("binOpSumBool",   VErr(NotNumeric()), "=2 + TRUE"),

      ("binOpMul",       VDouble(6), "=2 * 3"),
      ("binOpMulString", VErr(NotNumeric()), "=2 * \"a\""),
      ("binOpMulBool",   VErr(NotNumeric()), "=2 * TRUE"),

      ("binOpConcat =",     VString("abc"), "=\"ab\"& \"c\""),
      ("binOpConcatDouble", VString("ab1"), "=\"ab\"& 1"),
      ("binOpConcatTrue",   VString("abTRUE"), "=\"ab\"& TRUE"),
      ("binOpConcatFalse",  VString("abFALSE"), "=\"ab\"& FALSE")
    ) ++ lst("binop =", List(
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
        (false, "=1 = TRUE")
      )) ++ lst("binop >", List(
        (true,  "=2>1"),
        (false, "=1>1"),
        (false, "=1>2"),
        (false, "=1>\"5\""),
        (false, "=1>TRUE"),
        (false, "=1>FALSE"),
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
        (false, "=FALSE<FALSE"),
        (false, "=TRUE<TRUE"),
        (false, "=TRUE<FALSE"),
        (true,  "=FALSE<TRUE"),
        (false, "=\"b\"<\"a\""),
        (true,  "=\"a\"<\"b\""),
        (false, "=\"b\"<\"b\"")
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

