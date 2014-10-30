
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

  @Parameters(name= "{0}")
  def data: ju.Collection[Array[jl.Object]] = {
    val list = new ju.ArrayList[Array[jl.Object]]()
    List[Tuple3[String, Value, AnyRef]](
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
      ("binOpConcatFalse",  VString("abFALSE"), "=\"ab\"& FALSE"),

      ("binOp eq bool", VBool(true), "=TRUE = TRUE"),
      ("binOp eq bool", VBool(false), "=FALSE = TRUE"),
      ("binOp eq bool", VBool(false), "=TRUE = FALSE"),
      ("binOp eq bool", VBool(true), "=FALSE = FALSE"),
      ("binOp eq double", VBool(true), "=1 = 1"),
      ("binOp eq double", VBool(false), "=1 = 2"),
      ("binOp eq string", VBool(true), "=\"a\" = \"a\""),
      ("binOp eq string", VBool(false), "=\"a\" = \"b\""),
      ("binOp eq combo", VBool(false), "=\"a\" = 1"),
      ("binOp eq combo", VBool(false), "=\"a\" = TRUE"),
      ("binOp eq combo", VBool(false), "=1 = TRUE"),

      ("unOpPlus",          VDouble(5), "=+5"),
      ("unOpPlusString",    VErr(NotNumeric()), "=+\"A\""),
      ("unOpMin",           VDouble(-5), "=-5"),
      ("unOpMinString",     VErr(NotNumeric()), "=-\"A\""),
      ("unOpPercent",       VDouble(0.25), "=25%"),
      ("unOpPercent2",      VDouble(0.0025), "=25%%"),
      ("unOpPercentString", VErr(NotNumeric()), "=\"A\"%")

    ) foreach (x => list.add(Array(x._1, x._2, x._3)))
    return list
  }


}

